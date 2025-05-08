library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(DT)
library(bslib)
library(shinyjs) # For enhanced UI interactions
library(shinydashboard) # For better UI components
library(htmltools) # For better HTML handling
library(DBI)
library(duckdb)
library(ggiraph)
library(shinycssloaders)

# Load the SQL server information:
readRenviron(".Renviron")

# Define local temp dir for the downloaded data:
temp_dir <- tempdir()


# Create a better UI with dashboardPage and tabbed interface
ui <- dashboardPage(

  # Main app body here:
  dashboardHeader(title = "HAB Data Explorer"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("HAB Advisories Map", tabName = "habAdvisories", icon = icon("triangle-exclamation")),
                menuItem("Cyanotoxin Explorer", tabName = "cyanotoxinData", icon = icon("flask")),
                hr(),
                conditionalPanel(
                  condition = "input.tabs == 'habAdvisories'",
                  dateRangeInput("dateRange", "Select Date Range:", 
                                 start = Sys.Date() - 30, end = Sys.Date(), format = "mm-dd-yyyy"),
                  textInput("searchLocation", "Search Location:", ""),
                  checkboxInput("showActiveOnly", "Show Active HABs Only", FALSE),
                  actionButton("resetMap", "Reset View", icon = icon("undo")),
                  hr(),
                  helpText("HAB = Harmful Algal Bloom"),
                  helpText("Red markers indicate active advisories"),
                  helpText("Blue markers indicate past advisories")
                ),
                conditionalPanel(
                  condition = "input.tabs == 'cyanotoxinData'",
                  # Message about the database being used
                  htmlOutput("db_message"),
                  br(),
                  dateRangeInput("cyanoDateRange", "Date Range:", 
                                 start = Sys.Date() - 365, end = Sys.Date(), format = "mm-dd-yyyy"),
                  selectInput("toxinType", "Toxin Type:",
                              choices = c("Microcystin" = "microcystin",
                                          "Cylindrospermopsin" = "cylindro",
                                          "Anatoxin-a" = "anatox",
                                          "Saxitoxin" = "saxitox",
                                          "Phycocyanin" = "phyco"),
                              selected = "mcx"),
                  actionButton("refreshCyanoData", "Refresh Data", icon = icon("sync")),
                  actionButton("resetCyanoMap", "Reset View", icon = icon("undo")),
                  hr(),
                  helpText("Click on a site in the map to view its data")
                )
    )
  ),
  dashboardBody(
    useShinyjs(), # Initialize shinyjs
    
    # First, define the data loading overlay:
    div(
      id = "loading-overlay",
      style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; 
           background-color: rgba(255, 255, 255, 0.8); z-index: 1000; 
           display: none; align-items: center; justify-content: center;",
      h3(id = "loading-message", "Loading data, please wait...", style = "color: #555;")
    ),
    
    tabItems(
      # HAB Advisories Tab
      tabItem(tabName = "habAdvisories",
              fluidRow(
                column(width = 10, offset = 1,
                box(width = 12, 
                    leafletOutput("advisoriesMap", height = 500),
                    status = "primary", solidHeader = TRUE, title = "HAB Advisories Map"
                )
                )
              ),
              
              fluidRow(
                column(width = 10, offset = 1,
                box(width = 12,
                    h4("HAB Advisories Details"),
                    DT::dataTableOutput("habTable"),
                    status = "info", solidHeader = TRUE,
                    downloadButton("downloadData", "Download Data")
                )
                )
              )
      ),
      
      # Cyanotoxin Data Explorer Tab
      tabItem(tabName = "cyanotoxinData",
              fluidRow(
                column(width = 6,
                box(width = 12,
                    leafletOutput("cyanoMap", height = 500),
                    status = "primary", solidHeader = TRUE, title = "Choose a site to diplay cyanotoxin data"
                )
                ),
                column(width = 6,
                       box(width = 12,
                           withSpinner(girafeOutput("cyanoPlot", height = 500)),
                           status = "info", solidHeader = TRUE, title = "Cyanotoxin concentration over time"
                       )
                       )
              ),
              fluidRow(
                column(width = 8, offset = 2,
                box(width = 12,
                    DT::dataTableOutput("cyanoTable"),
                    status = "info", solidHeader = TRUE, title = "Cyanotoxin Data",
                    downloadButton("downloadCyanoData", "Download Data")
                )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  database_path <- reactiveVal(NULL)
  
  
  # Data work ----
  # Improved error handling for data loading
  all_sites <- reactive({
    req(file.exists("Data/all_site_info.csv"))
    tryCatch({
      read.csv("Data/all_site_info.csv")
    }, error = function(e) {
      showNotification("Error loading site data", type = "error")
      data.frame()
    })
  })
  
  habs_raw <- reactive({
    req(file.exists("Data/hab_advisory_database.csv"))
    tryCatch({
      read.csv("Data/hab_advisory_database.csv")
    }, error = function(e) {
      showNotification("Error loading HAB data", type = "error")
      data.frame()
    })
  })
  
  # Process HAB data
  habs <- reactive({
    req(nrow(habs_raw()) > 0, nrow(all_sites()) > 0)
    
    habs_raw() %>% 
      left_join(all_sites(), by = join_by(SiteID == MonitoringLocationIdentifier)) %>%
      mutate(
        Latitude = coalesce(Latitude, LatitudeMeasure),
        Longitude = coalesce(Longitude, LongitudeMeasure),
        DatePosted = mdy(DatePosted),
        DateLifted = mdy(DateLifted),
        Status = ifelse(is.na(DateLifted), "Active", "Lifted")
      ) %>%
      select(DatePosted, DateLifted, WaterbodyName, County, Description, Status, 
             SiteID, Latitude, Longitude, AU)
  })
  
  # Reactive data filtering with additional options
  filtered_data <- reactive({
    req(habs())
    data <- habs()
    
    # Apply date filter
    if (!is.null(input$dateRange)) {
      if (input$showActiveOnly) {
        # Just show active HABs
        data <- data %>% filter(is.na(DateLifted))
      } else {
        # Show HABs active during the period or posted during the period
        data <- data %>% filter(
          # Active HABs
          is.na(DateLifted) |
            # HABs that were active during selected period
            (DatePosted <= input$dateRange[2] & 
               (is.na(DateLifted) | DateLifted >= input$dateRange[1]))
        )
      }
    }
    
    # Apply location search if entered
    if (!is.null(input$searchLocation) && input$searchLocation != "") {
      data <- data %>% 
        filter(grepl(input$searchLocation, WaterbodyName, ignore.case = TRUE) |
                 grepl(input$searchLocation, County, ignore.case = TRUE))
    }
    
    return(data)
  })
  
  ### ADVISORY MAP OUTPUT ####
  output$advisoriesMap <- renderLeaflet({
    map_data <- filtered_data()
    
    # Create base map with selected basemap
    base_map <- leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Street") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      setView(lng = -92.3731, lat = 34.7465, zoom = 7) %>%  # Default to Arkansas
      addLayersControl(
        baseGroups = c("Street", "Satellite"),
        position = "topleft",
        options = layersControlOptions(collapsed = FALSE)
      )
    
    # Count active HABs
    active_habs_count <- sum(map_data$Status == "Active", na.rm = TRUE)
    
    # Create legend for map
    base_map <- base_map %>%
      addLegend(
        position = "bottomright",
        colors = c("red", "blue"),
        labels = c("Active HAB Advisory", "Past HAB Advisory"),
        opacity = 0.8
      )
    
    if (nrow(map_data) > 0) {
      # Add markers to map with improved popups
      base_map <- base_map %>%
        addCircleMarkers(
          data = map_data,
          lng = ~Longitude, lat = ~Latitude, 
          radius = 6,
          stroke = TRUE,
          weight = 2,
          color = ~ifelse(Status == "Active", "darkred", "darkblue"),
          fillColor = ~ifelse(Status == "Active", "red", "blue"),
          fillOpacity = 0.8,
          popup = ~paste(
            "<strong>Location:</strong>", WaterbodyName, 
            "<br><strong>County:</strong>", County,
            "<br><strong>Status:</strong>", 
            ifelse(Status == "Active", 
                   "<span style='color:red;font-weight:bold'>Active</span>", 
                   "<span style='color:blue'>Lifted</span>"),
            "<br><strong>Date Posted:</strong>", format(DatePosted, "%m-%d-%Y"),
            "<br><strong>Date Lifted:</strong>", 
            ifelse(is.na(DateLifted), "Still Active", format(DateLifted, "%m-%d-%Y")),
            "<br><strong>Description:</strong>", Description,
            "<br><strong>Site ID:</strong>", SiteID
          )
        )
      
      # Add info popup with number of HABs - with proper grammar
      if (active_habs_count == 0) {
        message_html <- paste0('<div style="text-align:center; font-weight:bold;">',
                               '<p style="font-size:18px; color:blue;">No active HAB advisories posted</p>',
                               '<button onclick="this.parentNode.parentNode.style.display=\'none\'; return false;" ',
                               'style="padding:8px 12px; background-color:#4CAF50; color:white; border:none; ',
                               'border-radius:4px; cursor:pointer;">See Map</button></div>')
      } else if (active_habs_count == 1) {
        message_html <- paste0('<div style="text-align:center; font-weight:bold;">',
                               '<p style="font-size:18px; color:red;">There is currently 1 active HAB advisory</p>',
                               '<button onclick="this.parentNode.parentNode.style.display=\'none\'; return false;" ',
                               'style="padding:8px 12px; background-color:#4CAF50; color:white; border:none; ',
                               'border-radius:4px; cursor:pointer;">See Map</button></div>')
      } else {
        message_html <- paste0('<div style="text-align:center; font-weight:bold;">',
                               '<p style="font-size:18px; color:red;">There are currently ', 
                               active_habs_count, ' active HAB advisories</p>',
                               '<button onclick="this.parentNode.parentNode.style.display=\'none\'; return false;" ',
                               'style="padding:8px 12px; background-color:#4CAF50; color:white; border:none; ',
                               'border-radius:4px; cursor:pointer;">See Map</button></div>')
      }
      
      # Add popup to map
      base_map <- base_map %>% addPopups(
        lng = -92.3731, lat = 34.7465, 
        popup = HTML(message_html)
      )
    } else {
      # Add info popup when no HABs found
      message_html <- paste0('<div style="text-align:center; font-weight:bold;">',
                             '<p style="font-size:18px; color:green;">No HAB advisories found for selected criteria</p>',
                             '<button onclick="this.parentNode.parentNode.style.display=\'none\'; return false;" ',
                             'style="padding:8px 12px; background-color:#4CAF50; color:white; border:none; ',
                             'border-radius:4px; cursor:pointer;">See Map</button></div>')
      
      base_map <- base_map %>% addPopups(
        lng = -92.3731, lat = 34.7465, 
        popup = HTML(message_html)
      )
    }
    
    return(base_map)
  })
  
  # Observe reset map button
  observeEvent(input$resetMap, {
    leafletProxy("advisoriesMap") %>% 
      setView(lng = -92.3731, lat = 34.7465, zoom = 7)
  })
  
  ### HAB DATA TABLE OUTPUT ###
  output$habTable <- DT::renderDataTable({
    map_data <- filtered_data()
    
    # If no data, return empty data frame with column names
    if(nrow(map_data) == 0) {
      return(data.frame(
        Waterbody = character(),
        County = character(),
        Status = character(),
        DatePosted = character(),
        DateLifted = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    # Format data for table display - depends on map data
    table_data <- map_data %>%
      mutate(
        DatePosted = format(DatePosted, "%m-%d-%Y"),
        DateLifted = ifelse(is.na(DateLifted), "Active", format(DateLifted, "%m-%d-%Y"))
      ) %>%
      select(
        Waterbody = WaterbodyName,
        County,
        Status,
        `Date Posted` = DatePosted,
        `Date Lifted` = DateLifted
      )
    
    return(table_data)
  }, options = list(
    pageLength = 10,
    lengthMenu = c(5, 10, 25, 50),
    searching = TRUE,
    scrollX = TRUE,
    dom = 'lftip', # Includes length changing input, filtering input, table, info and pagination
    language = list(
      emptyTable = "No HAB advisories found for selected criteria"
    ),
    rowCallback = DT::JS("function(row, data, index) {
      if(data[2] === 'Active') {
        $(row).addClass('danger');
        $('td', row).eq(2).css('color', 'red').css('font-weight', 'bold');
      }
    }")
  ))
  
  # Download handler for the table data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("hab_advisories_", format(Sys.Date(), "%Y-%m-%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  
  ## Cyanotoxin Explorer tab stuff:
  
  #### Data download and cleaner here ####
  # Step 1: Function to Fetch data from server and save tables to local database
  # Function only runs once initially, when Observe() calls it below
  fetch_data <- reactive({
    runjs('document.getElementById("loading-overlay").style.display = "flex";')
    runjs('document.getElementById("loading-message").textContent = "Fetching data from server, please wait...";')
    
    server_grab <- FALSE
    database_file <- NULL
    db_message <- NULL
    
    tryCatch({
      server_con <- dbConnect(
        odbc::odbc(),
        Driver   = "SQL Server",
        Server = Sys.getenv("SQL_SERVER"), #Testing, fix this 
        Database = Sys.getenv("SQL_DATABASE"),
        Trusted_Connection = "Yes")
      
      # Relevant toxin parameter names:
      # Anatoxin (ug/l)
      # Cylindrospermopsin (ug/l)
      # Microcystins (ug/l)
      # Saxitoxin (ug/l)
      # Phycocyanin (ug/l)

      # Pull only relevant results (toxin parameters):
     # WebLIMSResults <- dbReadTable(server_con, "WebLIMSResults") # THis reads in all data
      WebLIMSResults <- dbGetQuery(
        server_con,
        "SELECT *
          FROM WebLIMSResults
          WHERE WebParameter IN ('Anatoxin (ug/l)', 'Cylindrospermopsin (ug/l)', 'Microcystins (ug/l)',
        'Saxitoxin (ug/l)', 'Phycocyanin (ug/l)')"
      )
      
      # Get all stations that have relevant data:
      # WebLIMSStations  <- dbReadTable(server_con, "WebLIMSStations") # This reads in all stations
      WebLIMSStations <- dbGetQuery(server_con, "
    SELECT DISTINCT s.*
    FROM WebLIMSStations s
    INNER JOIN WebLIMSResults r ON s.StationID = r.StationID
    WHERE WebParameter IN ('Anatoxin (ug/l)', 'Cylindrospermopsin (ug/l)', 'Microcystins (ug/l)',
        'Saxitoxin (ug/l)', 'Phycocyanin (ug/l)')")
      
      server_grab <- TRUE
      
      # Close the SQL connection
      dbDisconnect(server_con)
      
      # Save tables to temp_dir duckDB database - use this connection for rest of app!
      runjs('document.getElementById("loading-message").textContent = "Initializing database...";')
      
      # Create duckDB database for data
      full_con <- dbConnect(duckdb::duckdb(), paste0(temp_dir, "/weblims_cyano.duckdb"))
      
      # Convert Date to character for db compatibility (must convert back to date whenever read in):
      WebLIMSResults$DateSampled <- as.character(WebLIMSResults$DateSampled)
      
      # Additional data cleaning work:
      WebLIMSResults <- WebLIMSResults %>% 
        mutate(
          WebResult = as.character(WebResult),
          WebResult = gsub("[^0-9.<>=]", "", WebResult),  # Remove any non-numeric characters except <, >, .
          DL = case_when(
            grepl("^>", WebResult) ~ ">DL",
            grepl("^<", WebResult) ~ "<DL",
            TRUE ~ "Measured value"
          ),
          WebResult = case_when(
            grepl("^>", WebResult) ~ suppressWarnings(as.numeric(sub(">", "", WebResult))),
            grepl("^<", WebResult) ~ suppressWarnings(as.numeric(sub("<", "", WebResult))) / 2,
            TRUE ~ suppressWarnings(as.numeric(trimws(WebResult)))
          ),
          Qualifier = if_else(is.na(Qualifier) | Qualifier == "", "None", Qualifier),  # Handle empty and NA values
        ) %>%
        filter(!is.na(WebResult)) %>%  # Remove rows where FinalResult is NA
        mutate(across(c(Qualifier, DL), as.factor))
      
      # Saving R dataframes to the database:
      dbWriteTable(full_con, "WebLIMSResults", WebLIMSResults, overwrite = TRUE)
      dbWriteTable(full_con, "WebLIMSStations", WebLIMSStations, overwrite = TRUE)
      
      rm(WebLIMSResults)
      rm(WebLIMSStations)
      
      dbDisconnect(full_con)
      
      database_file <- paste0(temp_dir, "/weblims_cyano.duckdb")
      
    }, error = function(e) {
      message("Error fetching or initializing data.:", e$message)
    })
    
    
    if (!server_grab) {
      # code here runs IF the SQL Server data grab FAILS:
      runjs('document.getElementById("loading-message").textContent = "Failed to fetch the data... use backup here?";')
      # 
      
    } else {
      # Code here runs IF the SQL Server data grab SUCCEEDS:
      # Fetch the date range from duckDB
      full_con <- dbConnect(duckdb::duckdb(), database_file)
      
      # Rename StationID to SamplingPoint to reduce errors:
      dbExecute(full_con, "ALTER TABLE WebLIMSResults RENAME COLUMN StationID to SamplingPoint")
      
      date_range <- dbGetQuery(full_con, "SELECT MIN(DateSampled) AS min_date, MAX(DateSampled) AS max_date FROM WebLIMSResults")
      # FIX the leading/trailing spaces for duckDB here:
      # Run update queries to trim spaces
      dbExecute(full_con, "UPDATE WebLIMSResults SET SamplingPoint = TRIM(SamplingPoint);")
      dbExecute(full_con, "UPDATE WebLIMSStations SET StationID = TRIM(StationID);")
      
      
      
      dbDisconnect(full_con)
      
      db_message <- paste("Using most recent version of the database:",
                          "<br>", "Data available between ", format(as.Date(date_range$min_date), "%m-%d-%Y"), " and ", format(as.Date(date_range$max_date), "%m-%d-%Y"))
    }
    
    runjs('document.getElementById("loading-overlay").style.display = "none";')  # Hide overlay after success
    
    output$db_message <- renderUI({
      HTML(db_message)
    })
    # Output is path to database file (full duckDB - see WebLIMS-data-visualizer app for backup db usage)
    database_file
  })
  
  # Calling the fetch_data function (only happens once initially):
  observeEvent(TRUE, {
    db_file <- fetch_data()
    database_path(db_file)
  }, once = TRUE)
  
  # Getting data for map:
  cyan_data <- reactive({
    req(input$toxinType)
    
    database_file <- database_path()
    
    con <- dbConnect(duckdb(), database_file)
    
    query <- paste0(
      "SELECT SamplingPoint, DateSampled, WebResult, Units, Qualifier, ",
      "WebParameter, DL, WebLIMSStations.Description, WebLIMSStations.Latitude, WebLIMSStations.Longitude ",
      "FROM WebLIMSResults ",
      "LEFT JOIN WebLIMSStations ON WebLIMSResults.SamplingPoint = WebLIMSStations.StationID"#,
      # "WHERE SamplingPoint = '", input$site, "' ",
      # "AND WebParameter = '", input$parameter, "'"
    )
    
    raw_data <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    # Filter based on toxin type (case-insensitive match)
    filtered_data <- raw_data %>%
      filter(grepl(input$toxinType, WebParameter, ignore.case = TRUE))
    
    # Return filtered data object
    filtered_data
  })
  

  
  
  ### Cyanotoxin map ###
  output$cyanoMap <- renderLeaflet({
    # Create base map with selected basemap
    base_map <- leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Street") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      setView(lng = -92.3731, lat = 34.7465, zoom = 7) %>%  # Default to Arkansas
      addLayersControl(
        baseGroups = c("Street", "Satellite"),
        position = "topleft",
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
    map_data <- cyan_data()
    # TESTING ----------------------
    # browser()
    
    # Add markers to the map from WebLIMSStations:
    base_map <- base_map %>%
      addCircleMarkers(
        data = map_data,
        lng = ~Longitude, lat = ~Latitude,
        radius = 6,
        stroke = TRUE,
        weight = 2,
        layerId = ~SamplingPoint, # ID to track site clicks
        # color = ~ifelse(Status == "Active", "darkred", "darkblue"),
        # fillColor = ~ifelse(Status == "Active", "red", "blue"),
        # fillOpacity = 0.8,
        popup = ~paste(
          "<strong>Location:</strong>", Description,
          "<br><strong>Site ID:</strong>", SamplingPoint
        )
      )
    
  })
  
  ### PLOTTING THE CLICKED SITE DATA ####
  
  # Reactive to get clicked site:
  clicked_site <- reactive({
    req(input$cyanoMap_marker_click)
    input$cyanoMap_marker_click$id
  })
  
  # Filtering data to clicked site:
  site_data <- reactive({
    req(clicked_site())
    cyan_data() %>% filter(SamplingPoint == clicked_site()) %>% 
      mutate(DateSampled = as.Date(DateSampled))
  })
  
  # PLOTTING:
  output$cyanoPlot <- renderGirafe({
    req(nrow(site_data()) > 0)
    
    p <- ggplot(site_data(), aes(x = DateSampled, y = WebResult,
                                 tooltip = paste("Date:", format(DateSampled, "%m-%d-%Y"),
                                                 "<br>Result:", WebResult,
                                                 #"<br>Value:", DL, # weird display issue 
                                                 "<br>Qualifiers:", Qualifier)
                                 )) +
      geom_point_interactive(aes(color = Qualifier, shape = DL),
                             alpha = 0.7,
                             size = 2.5) +
      scale_shape_manual(values = c("Measured value" = 16, "<DL" = 17, ">DL" = 17)) + # 16 = circle, 17 = triangle
      theme_classic(base_size = 14) +
      labs(
        title = paste("Toxin Levels at", clicked_site()),
        x = "Date",
        y = "Result",
        color = "Qualifiers",
        shape = "Values"
      ) +
      theme( axis.text.x = element_text(angle = 45, hjust =1))
    
    girafe(ggobj = p)
  })
  
  
  
}

shinyApp(ui, server)