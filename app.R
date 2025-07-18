library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(DT)
library(bslib)
library(shinyjs)
library(shinydashboard) 
library(htmltools)
library(DBI)
library(duckdb)
library(ggiraph)
library(shinycssloaders)
library(ggplot2)

# Load the SQL server information:
readRenviron(".Renviron")

# Define local temp dir for the downloaded data:
temp_dir <- tempdir()
fallback_data_path <- "WebLIMS_backup.duckdb" # Update this with backup database


# Create a better UI with dashboardPage and tabbed interface
ui <- dashboardPage(

  # Main app body here:
  dashboardHeader(title = "HAB Data Explorer"),
  dashboardSidebar(
    width = 270,
    sidebarMenu(id = "tabs",
                menuItem("HAB Advisories Map", tabName = "habAdvisories", icon = icon("triangle-exclamation")),
                
                # Comment/uncomment this single line below to hide/show the cyanotoxin tab in the app.
                # If NOT using the cyanotoxin explorer tab, can also uncomment the entire cyanotoxin 
                # explorer part of the Server to make the app run faster:
                
                 menuItem("Cyanotoxin Explorer", tabName = "cyanotoxinData", icon = icon("flask")),
               
                 hr(),
                
                # Panel for HAB Advisories tab:
                conditionalPanel(
                  condition = "input.tabs == 'habAdvisories'",
                  dateRangeInput("dateRange", "Filter to a Date Range:", format = "mm-dd-yyyy"),
                  textInput("searchLocation", "Search by Lake Name:", ""),
                  checkboxInput("showActiveOnly", "Show Active HABs Only", FALSE),
                  fluidRow(
                    column(width = 4,
                           actionButton("resetFilters", "Reset Filters", icon = icon("undo"))
                    ),
                    
                    column(width = 1),
                    column(width = 4,
                           actionButton("resetMap", "Reset Map", icon = icon("crosshairs"))
                    )
                  ),
        
                  hr(),
                  # helpText("HAB = Harmful Algal Bloom"),
                  # helpText("Red markers indicate active advisories"),
                  # helpText("Blue markers indicate past advisories")
                ),
                conditionalPanel(
                  # Panel for Cyantoxin explorer tab:
                  condition = "input.tabs == 'cyanotoxinData'",
                  # Message about the database being used
                  htmlOutput("db_message"),
                  br(),
                  dateRangeInput("cyanoDateRange", "Date Range:", 
                                 start = Sys.Date() - 365, end = Sys.Date(), format = "mm-dd-yyyy"),
                  selectInput("toxinType", "Toxin Type:",
                              choices = c("Microcystin" = "Microcystin",
                                          "Cylindrospermopsin" = "Cylindrospermopsin"),
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
                    status = "primary", solidHeader = TRUE, title = "Arkansas HAB Advisories Map"
                )
                )
              ),
              
              fluidRow(
                column(width = 10, offset = 1,
                box(width = 12,
                    h4("Arkansas HAB Advisories Details"),
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
                           div(
                             style = "height: auto; max-height: 500px;  overflow: hidden;",
                             withSpinner(uiOutput("cyanoPlotUI"))
                           ),
                           status = "info", solidHeader = TRUE, title = "Cyanotoxin concentration over time",
                       )
                       )
              ),
              fluidRow(
                column(width = 8, offset = 2,
                box(width = 12,
                    # DT::dataTableOutput("cyanoTable"),
                    div(style = "overflow-x: auto;", DT::dataTableOutput("cyanoTable")),
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
  
  selected_site <- reactiveVal(NULL)
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
       #read.csv("Data/current_hab_test.csv") # Fake current data for testing
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
             SiteID, Latitude, Longitude)
  })
  
  # Observe the date range of Advisories and default to range in data:
  observe({
    req(habs())
    min_date <- min(habs()$DatePosted, na.rm = TRUE)
    max_date <- max(coalesce(habs()$DateLifted, Sys.Date()), na.rm = TRUE)
    updateDateRangeInput(session, "dateRange", start = min_date, end = max_date)
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
  
  #### ADVISORIES MAP #######
  ##############################################################################
  output$advisoriesMap <- renderLeaflet({
    map_data <- filtered_data()
    
    # Group by site to combine all advisories at the same location
    site_summary <- map_data %>%
      group_by(SiteID, Latitude, Longitude, WaterbodyName, County) %>%
      arrange(desc(DatePosted)) %>%
      summarise(
        popup_text = paste0(
          "<strong>Location:</strong> ", first(WaterbodyName),
          "<br><strong>Site ID:</strong> ", first(SiteID),
          "<br><strong>County:</strong> ", first(County),
          "<br><strong>Description:</strong> ", first(Description),

          "<br><br><strong>Advisory History:</strong><ul>",
          paste0(
            "<li>",
            "<strong>Status:</strong> ",
            ifelse(Status == "Active",
                   "<span style='color:red;font-weight:bold'>Active</span>",
                   "<span style='color:#0080b7'>Lifted</span>"),
            "<ul style='margin-top:4px; margin-bottom:4px;'>",
            "<li><strong>Date Posted:</strong> ", format(DatePosted, "%m-%d-%Y"), "</li>",
            "<li><strong>Date Lifted:</strong> ",
            ifelse(is.na(DateLifted), "Still Active", format(DateLifted, "%m-%d-%Y")),
            "</li>",
            "</ul></li>"
          ) %>% paste(collapse = ""),
          "</ul>"
        ),
        status_color = if_else(any(Status == "Active"), "red", "#0080b7"),
        .groups = "drop"
      )
    
    # Count active HABs
    active_habs_count <- sum(map_data$Status == "Active", na.rm = TRUE)
    
    # Base map setup
    base_map <- leaflet() %>%
      addProviderTiles("OpenStreetMap", group = "Street") %>%
      addProviderTiles("USGS.USImagery", group = "Satellite") %>%
      setView(lng = -92.3731, lat = 34.7465, zoom = 7) %>%
      addLayersControl(
        baseGroups = c("Street", "Satellite"),
        position = "topleft",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("red", "#0080b7"),
        labels = c("Active HAB Advisory", "Past HAB Advisory"),
        opacity = 0.8
      )
    
    if (nrow(site_summary) > 0) {
      base_map <- base_map %>%
        addCircleMarkers(
          data = site_summary,
          lng = ~Longitude, lat = ~Latitude,
          radius = 8,
          stroke = TRUE,
          weight = 2,
          color = ~status_color,
          fillColor = ~status_color,
          fillOpacity = 0.8,
          popup = ~popup_text,
          layerId = ~WaterbodyName
        )
      
      # Info popup message
      if (active_habs_count == 0) {
        message_html <- paste0(
          '<div style="text-align:center; font-weight:bold;">',
          '<p style="font-size:18px; color:#0080b7;">There are no active HAB advisories in effect.</p>',
          '<button onclick="this.parentNode.parentNode.style.display=\'none\'; return false;" ',
          'style="padding:8px 12px; background-color:#4CAF50; color:white; border:none; ',
          'border-radius:4px; cursor:pointer;">See previous advisories</button></div>')
      } else if (active_habs_count == 1) {
        message_html <- paste0(
          '<div style="text-align:center; font-weight:bold;">',
          '<p style="font-size:18px; color:red;">There is currently 1 active HAB advisory.</p>',
          '<button onclick="this.parentNode.parentNode.style.display=\'none\'; return false;" ',
          'style="padding:8px 12px; background-color:#4CAF50; color:white; border:none; ',
          'border-radius:4px; cursor:pointer;">See Map</button></div>')
      } else {
        message_html <- paste0(
          '<div style="text-align:center; font-weight:bold;">',
          '<p style="font-size:18px; color:red;">There are currently ', 
          active_habs_count, ' active HAB advisories.</p>',
          '<button onclick="this.parentNode.parentNode.style.display=\'none\'; return false;" ',
          'style="padding:8px 12px; background-color:#4CAF50; color:white; border:none; ',
          'border-radius:4px; cursor:pointer;">See Map</button></div>')
      }
      
      base_map <- base_map %>% addPopups(
        lng = -92.3731, lat = 34.7465,
        popup = HTML(message_html)
      )
    } else {
      message_html <- paste0(
        '<div style="text-align:center; font-weight:bold;">',
        '<p style="font-size:18px; color:#0080b7;">No HAB advisories found for selected criteria</p>',
        '<button onclick="this.parentNode.parentNode.style.display=\'none\'; return false;" ',
        'style="padding:8px 12px; background-color:#4CAF50; color:white; border:none; ',
        'border-radius:4px; cursor:pointer;">See Map</button></div>')
      
      base_map <- base_map %>% addPopups(
        lng = -92.3731, lat = 34.7465,
        popup = HTML(message_html)
      )
    }
    
    base_map
  })
  
  
  # Observe map marker clicks:
  observeEvent(input$advisoriesMap_marker_click, {
    click <- input$advisoriesMap_marker_click
    if (!is.null(click$id)) {
      selected_site(click$id)
    }
  })
  
  # Observe reset map button
  observeEvent(input$resetMap, {
    selected_site(NULL)
    leafletProxy("advisoriesMap") %>%
      setView(lng = -92.3731, lat = 34.7465, zoom = 7)
    
    runjs("
    document.querySelectorAll('.leaflet-control-layers-base label').forEach(function(label) {
      if(label.textContent.trim() === 'Street') {
        label.querySelector('input').click();
      }
    });
  ")
  })
  
  
  # Reset filters button
  observeEvent(input$resetFilters, {
    selected_site(NULL)
    updateTextInput(session, "searchLocation", value = "")
    updateCheckboxInput(session, "showActiveOnly", value = FALSE)
    # Reset date range to full
    if (nrow(habs()) > 0) {
      min_date <- min(habs()$DatePosted, na.rm = TRUE)
      max_date <- max(coalesce(habs()$DateLifted, Sys.Date()), na.rm = TRUE)
      updateDateRangeInput(session, "dateRange", start = min_date, end = max_date)
    }
  })
  
  
  
  ### HAB DATA TABLE OUTPUT ###
  output$habTable <- DT::renderDataTable({
    map_data <- filtered_data()
    
    # If site selected from the map, further filter to that site
    if (!is.null(selected_site())) {
      map_data <- map_data %>% filter(WaterbodyName == selected_site())
    }
    
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
  
  
  #### Cyanotoxin Explorer tab stuff: ######
  ###################################################################################
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
        Server = Sys.getenv("SQL_SERVER_FAILS"), #Testing, fix this
        Database = Sys.getenv("SQL_DATABASE"),
        Trusted_Connection = "Yes")

      # Relevant toxin parameter names:
      # Anatoxin (ug/l) - Don't use
      # Cylindrospermopsin (ug/l)
      # Microcystins (ug/l)
      # Saxitoxin (ug/l) - Don't use
      # Phycocyanin (ug/l) - Don't use

      # Pull only relevant results (toxin parameters):
     # WebLIMSResults <- dbReadTable(server_con, "WebLIMSResults") # THis reads in all data
      WebLIMSResults <- dbGetQuery(
        server_con,
        "SELECT *
          FROM WebLIMSResults
          WHERE WebParameter IN ('Cylindrospermopsin (ug/l)', 'Microcystins (ug/l)')"
      )

      # Get all stations that have relevant data:
      # WebLIMSStations  <- dbReadTable(server_con, "WebLIMSStations") # This reads in all stations
      WebLIMSStations <- dbGetQuery(server_con, "
    SELECT DISTINCT s.*
    FROM WebLIMSStations s
    INNER JOIN WebLIMSResults r ON s.StationID = r.StationID
    WHERE WebParameter IN ('Cylindrospermopsin (ug/l)', 'Microcystins (ug/l)')")

      server_grab <- TRUE

      # Close the SQL connection
      dbDisconnect(server_con)

      # Save tables to temp_dir duckDB database - use this connection for rest of app!
      runjs('document.getElementById("loading-message").textContent = "Initializing database...";')

      # Create an ephemeral duckDB database for data
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
          Qualifier = if_else(is.na(Qualifier) | Qualifier == "", "None", Qualifier), # Handle empty and NA values
          RelativeDepthSample = if_else(is.na(trimws(RelativeDepthSample)) | trimws(RelativeDepthSample) == "",
                                          "N/A",
                                          toupper(trimws(RelativeDepthSample)))
        ) %>%
        filter(!is.na(WebResult)) %>%  # Remove rows where FinalResult is NA
        mutate(across(c(Qualifier, RelativeDepthSample, DL), as.factor))

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
      runjs('document.getElementById("loading-message").textContent = "Failed to fetch latest database, using backup...";')
      
      # ENSURE backup database column names and types are identical to SQL Server!!!
      # For now, pulling in tables to R and doing work here to reduce errors with
      # altering the original/backup database
      
      conn_sqlite <- dbConnect(duckdb::duckdb(), fallback_data_path)
      
      # Rename StationID to SamplingPoint to reduce errors (need to do this once every
      # time the backup database is updated, then it will be set):
      cols <- dbListFields(conn_sqlite, "WebLIMSResults")
      if ("StationID" %in% cols) {
        dbExecute(conn_sqlite, "ALTER TABLE WebLIMSResults RENAME COLUMN StationID TO SamplingPoint")
      }
      
      # FIX the leading/trailing spaces for SQLITE here:
      dbExecute(conn_sqlite, "UPDATE WebLIMSResults SET SamplingPoint = TRIM(SamplingPoint);")
      dbExecute(conn_sqlite, "UPDATE WebLIMSStations SET StationID = TRIM(StationID);")
      
      # Since this is the full LIMS database, can filter to just the cyano data in SQL:
      WebLIMSResults <- dbGetQuery(conn_sqlite, "
        SELECT *
        FROM WebLIMSResults
        WHERE WebParameter IN ('Cylindrospermopsin (ug/l)', 'Microcystins (ug/l)')")
      
      WebLIMSStations <- dbGetQuery(conn_sqlite, "
        SELECT DISTINCT s.*
        FROM WebLIMSStations s
        INNER JOIN WebLIMSResults r ON s.StationID = r.SamplingPoint
        WHERE WebParameter IN ('Cylindrospermopsin (ug/l)', 'Microcystins (ug/l)')")
      
      dbDisconnect(conn_sqlite)
      
      # WebLIMSStations needs no cleaning
      
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
          Qualifier = if_else(is.na(Qualifier) | Qualifier == "", "None", Qualifier), # Handle empty and NA values
          RelativeDepthSample = if_else(is.na(trimws(RelativeDepthSample)) | trimws(RelativeDepthSample) == "",
                                        "N/A",
                                        toupper(trimws(RelativeDepthSample)))
        ) %>%
        filter(!is.na(WebResult)) %>%  # Remove rows where FinalResult is NA
        mutate(across(c(Qualifier, RelativeDepthSample, DL), as.factor))
      
      # Create an ephemeral duckDB database for data
      full_con <- dbConnect(duckdb::duckdb(), paste0(temp_dir, "/weblims_cyano.duckdb"))
      # Saving R dataframes to a temp database:
      dbWriteTable(full_con, "WebLIMSResults", WebLIMSResults, overwrite = TRUE)
      dbWriteTable(full_con, "WebLIMSStations", WebLIMSStations, overwrite = TRUE)
      dbDisconnect(full_con)
      
      database_file <- paste0(temp_dir, "/weblims_cyano.duckdb")
     
      
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

      db_message <- paste("Using most recent version", "<br>", "of the database. Data available",
                          "<br>", "between ", format(as.Date(date_range$min_date), "%m-%d-%Y"), " and ", format(as.Date(date_range$max_date), "%m-%d-%Y"))
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
      "WebParameter, DL, RelativeDepthSample, WebLIMSStations.Description, WebLIMSStations.Latitude, WebLIMSStations.Longitude ",
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
      addProviderTiles("OpenStreetMap", group = "Street") %>%
      addProviderTiles("USGS.USImagery", group = "Satellite") %>%
      setView(lng = -92.3731, lat = 34.7465, zoom = 7) %>%  # Default to Arkansas
      addLayersControl(
        baseGroups = c("Street", "Satellite"),
        position = "topleft",
        options = layersControlOptions(collapsed = FALSE)
      )


    map_data <- cyan_data()

     # Test with browser:
    # browser()

    # Add markers to the map from WebLIMSStations:
    base_map <- base_map %>%
      addCircleMarkers(
        data = map_data,
        lng = ~Longitude, lat = ~Latitude,
        radius = 8,
        stroke = TRUE,
        weight = 3,
        color = "#0DA5B5",
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

  # Placeholder text until user selects a site:
  output$cyanoPlotUI <- renderUI({
    if (is.null(input$cyanoMap_marker_click)) {
      tags$div("Select a site to display data", style = "text-align:center;
               color: #777; padding-top: 235px; padding-bottom: 235px; font-size: 20px;")
    } else {
      #girafeOutput("cyanoPlot", width = "100%", height = "100%")
      tags$div(
        style = "height: auto; width: 100%; max-width: 600px;",
        girafeOutput("cyanoPlot", width = "100%", height = "auto")
      )
      
    }
  })

  # PLOTTING:
  output$cyanoPlot <- renderGirafe({
    req(nrow(site_data()) > 0)

    # Check if RelativeDepthSample has any non-NA values
    use_size <- any(site_data()$RelativeDepthSample != "N/A")

    p <- ggplot(site_data(), aes(x = DateSampled, y = WebResult,
                                 tooltip = paste("Date:", format(DateSampled, "%Y-%m-%d"),
                                                 "<br>Result:", WebResult,
                                                 #"<br>Value:", DL, # weird display issue
                                                 "<br>Qualifiers:", Qualifier,
                                                 "<br>Relative Depth:", RelativeDepthSample))) +
      geom_point_interactive(aes(color = Qualifier, shape = DL),
                             alpha = 0.7,
                             size = 2.5) +
      scale_shape_manual(values = c("Measured value" = 16, "<DL" = 17, ">DL" = 17)) + # 16 = circle, 17 = triangle
      theme_classic(base_size = 14) +
      labs(
        title = paste(input$toxinType, "levels at", clicked_site()),
        x = "Date",
        y = expression(paste("Result (", mu, "g/L)")),
        color = "Qualifiers",
        shape = "Values"
      ) +
      scale_x_date(date_labels = "%Y-%m-%d") +
      theme( axis.text.x = element_text(angle = 45, hjust =1))

    # Add size mapping only if RelativeDepthSample is not all NA
    if (use_size) {
      p <- p +
        geom_point_interactive(aes(size = RelativeDepthSample, color = Qualifier, shape = DL), alpha = 0.7) +
        # scale_shape_manual(values = c("Measured value" = 16, "<DL" = 17, ">DL" = 17)) +
        scale_size_manual(values = c("EPILIMNION" = 2.5, "HYPOLIMNION" = 5.5,
                                     "THERMOCLINE" = 4, "MID-DEPTH" = 4, "N/A" = 7),
                          name = "Relative Depth",
                          drop = TRUE)
    }

    girafe(ggobj = p)
  })

  # Table for toxin tab:
  output$cyanoTable <- DT::renderDT ({
    req(nrow(site_data()) > 0)

    DT::datatable(site_data()[, c("SamplingPoint", "DateSampled","WebResult", "Units", "Qualifier", "WebParameter", "DL","RelativeDepthSample", "Description", "Latitude", "Longitude")],
                  colnames = c("Site", "Date","Result", "Units", "Qualifier", "Parameter", "DL","Relative Depth", "Description", "Latitude", "Longitude"),
                  extensions = 'Buttons',  # Enable buttons extension)
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      responsive = TRUE,
      searching = FALSE  # Disable the search function
    ),
    rownames = FALSE)
  })

  # Handle data downloader for toxin table:
  output$downloadCyanoData <- downloadHandler(
    filename = function() {
      paste0(clicked_site(), "_", input$toxinType, ".csv")
    },
    content = function(file) {
      write.csv(site_data()[, c("SamplingPoint", "DateSampled", "WebResult", "Units", "Qualifier", "WebParameter", "DL", "RelativeDepthSample", "Description", "Latitude", "Longitude")],
                file,
                row.names = FALSE)
    }
  )

  
}

shinyApp(ui, server)