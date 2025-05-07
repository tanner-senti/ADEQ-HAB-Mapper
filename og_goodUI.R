library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(DT)
library(bslib)
library(shinyjs) # For enhanced UI interactions
library(shinydashboard) # For better UI components
library(htmltools) # For better HTML handling

# Create a better UI with dashboardPage and tabbed interface
ui <- dashboardPage(
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
                  selectInput("mapType", "Map Type",
                              choices = c("OpenStreetMap" = "OpenStreetMap",
                                          "Esri.WorldImagery" = "Esri.WorldImagery",
                                          "CartoDB.Positron" = "CartoDB.Positron"),
                              selected = "OpenStreetMap"),
                  actionButton("resetMap", "Reset View", icon = icon("undo")),
                  hr(),
                  helpText("HAB = Harmful Algal Bloom"),
                  helpText("Red markers indicate active advisories"),
                  helpText("Blue markers indicate past advisories")
                ),
                conditionalPanel(
                  condition = "input.tabs == 'cyanotoxinData'",
                  dateRangeInput("cyanoDateRange", "Date Range:", 
                                 start = Sys.Date() - 365, end = Sys.Date(), format = "mm-dd-yyyy"),
                  selectInput("toxinType", "Toxin Type:",
                              choices = c("All Toxins" = "all",
                                          "Microcystin" = "microcystin",
                                          "Cylindrospermopsin" = "cylindrospermopsin",
                                          "Anatoxin-a" = "anatoxin",
                                          "Saxitoxin" = "saxitoxin"),
                              selected = "all"),
                  selectInput("cyanoMapType", "Map Type:",
                              choices = c("OpenStreetMap" = "OpenStreetMap",
                                          "Esri.WorldImagery" = "Esri.WorldImagery",
                                          "CartoDB.Positron" = "CartoDB.Positron"),
                              selected = "OpenStreetMap"),
                  actionButton("refreshCyanoData", "Refresh Data", icon = icon("sync")),
                  actionButton("resetCyanoMap", "Reset View", icon = icon("undo")),
                  hr(),
                  helpText("Click on a site in the map to view its data")
                )
    )
  ),
  dashboardBody(
    useShinyjs(), # Initialize shinyjs
    
    tabItems(
      # HAB Advisories Tab
      tabItem(tabName = "habAdvisories",
              fluidRow(
                box(width = 12, 
                    leafletOutput("map", height = 500),
                    status = "primary", solidHeader = TRUE, title = "HAB Advisories Map"
                )
              ),
              
              fluidRow(
                box(width = 12,
                    h4("HAB Advisories Details"),
                    DT::dataTableOutput("habTable"),
                    status = "info", solidHeader = TRUE,
                    downloadButton("downloadData", "Download Data")
                )
              )
      ),
      
      # Cyanotoxin Data Explorer Tab
      tabItem(tabName = "cyanotoxinData",
              fluidRow(
                box(width = 12,
                    leafletOutput("cyanoMap", height = 400),
                    status = "primary", solidHeader = TRUE, title = "Cyanotoxin Monitoring Sites"
                )
              ),
              
              fluidRow(
                box(width = 6,
                    plotlyOutput("cyanoPlot", height = 400),
                    status = "info", solidHeader = TRUE, title = "Cyanotoxin Concentration Over Time"
                ),
                box(width = 6,
                    uiOutput("siteInfo"),
                    status = "warning", solidHeader = TRUE, title = "Site Information"
                )
              ),
              
              fluidRow(
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

server <- function(input, output, session) {
  
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
  
  # Map output with improved features
  output$map <- renderLeaflet({
    map_data <- filtered_data()
    
    # Create base map with selected basemap
    base_map <- leaflet() %>%
      addProviderTiles(input$mapType) %>% 
      setView(lng = -92.3731, lat = 34.7465, zoom = 7) # Default to Arkansas
    
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
    leafletProxy("map") %>% 
      setView(lng = -92.3731, lat = 34.7465, zoom = 7)
  })
  
  # Enhanced data table with better handling
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
    
    # Format data for table display
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
}

shinyApp(ui, server)