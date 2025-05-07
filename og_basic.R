library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(DT)
library(bslib)


ui <- fluidPage(
  titlePanel("HAB Advisories Map"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Select Date Range:", 
                     start = Sys.Date() - 30, end = Sys.Date(), format = "mm-dd-yyyy"),
      textInput("searchLocation", "Search Location:", "")
    ),
    mainPanel(
      
      leafletOutput("map"),
      # Add a header and data table below map
      h4("HAB Advisories Details"),
      DT::dataTableOutput("habTable")
    )
    
  )
)

server <- function(input, output, session) {
  
  # Data work ----
  # Filling in missing site info
  all_sites <- read.csv("Data/all_site_info.csv")
  habs <- read.csv("Data/hab_advisory_database.csv")
  
  # Important: coordinates for X sites or HAB responses must be added manually to
  # the hab_advisory_database. Other sites with accurate SiteIDs can be automatically
  # added from all_site_info
  habs <- habs %>% 
    left_join(all_sites, by = join_by(SiteID == MonitoringLocationIdentifier)) %>%
    mutate(
      Latitude = coalesce(Latitude, LatitudeMeasure),
      Longitude = coalesce(Longitude, LongitudeMeasure),
      DatePosted = mdy(DatePosted),
      DateLifted = mdy(DateLifted)
    ) %>%
    select(DatePosted:Longitude, AU)
  
  # Test data for a current HAB, use regualr habs obj for no current HAB:
  habs2 <- read.csv("Data/current_hab_test.csv")
  habs2 <- habs2 %>% 
    mutate(DatePosted = as.Date(mdy(DatePosted)),
           DateLifted = as.Date(mdy(DateLifted)))
  
  # Reactive data filtering ----
  filtered_data <- reactive({
    habs %>% # CHANGE THIS AFTER TESTING
      filter(
        # Show active HABs OR historical HABs that fall in the selected date range
        (is.na(DateLifted) | (DatePosted >= input$dateRange[1] & DatePosted <= input$dateRange[2])) &
          grepl(input$searchLocation, WaterbodyName, ignore.case = TRUE)
      )
  })
  
  output$map <- renderLeaflet({
    map_data <- filtered_data()
    
    # Create base map
    base_map <- leaflet() %>%
      addTiles() %>% 
      setView(lng = -92.3731, lat = 34.7465, zoom = 7) # Default to Arkansas
    
    # Count active HABs
    active_habs_count <- sum(is.na(map_data$DateLifted))
    
    if (nrow(map_data) > 0) {
      # Add markers to map
      base_map <- base_map %>%
        addCircleMarkers(
          data = map_data,
          lng = ~Longitude, lat = ~Latitude, radius = 5,
          color = ~ifelse(is.na(DateLifted), "red", "blue"), # Active = red, Past = blue
          popup = ~paste("Location:", WaterbodyName, 
                         "<br>Date Posted:", format(DatePosted, "%m-%d-%Y"),
                         "<br>Date Lifted:", ifelse(is.na(DateLifted), "Active", format(DateLifted, "%m-%d-%Y")))
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
    } else {
      # Add info popup when no HABs found
      message_html <- paste0('<div style="text-align:center; font-weight:bold;">',
                             '<p style="font-size:18px; color:green;">No current HAB advisories posted</p>',
                             '<button onclick="this.parentNode.parentNode.style.display=\'none\'; return false;" ',
                             'style="padding:8px 12px; background-color:#4CAF50; color:white; border:none; ',
                             'border-radius:4px; cursor:pointer;">See Map</button></div>')
    }
    
    # Add popup to map
    base_map %>% addPopups(
      lng = -92.3731, lat = 34.7465, 
      popup = HTML(message_html)
    )
  })
  
  # Data table below map:
  output$habTable <- DT::renderDataTable({
    map_data <- filtered_data()
    
    # If no data, return empty data frame with column names
    if(nrow(map_data) == 0) {
      return(data.frame(
        Waterbody = character(),
        Status = character(),
        DatePosted = character(),
        DateLifted = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    # Format data for table display
    table_data <- map_data %>%
      mutate(
        Status = ifelse(is.na(DateLifted), "Active", "Lifted"),
        DatePosted = format(DatePosted, "%m-%d-%Y"),
        DateLifted = ifelse(is.na(DateLifted), "Active", format(DateLifted, "%m-%d-%Y"))
      ) %>%
      select(
        Waterbody = WaterbodyName,
        Status,
        `Date Posted` = DatePosted,
        `Date Lifted` = DateLifted
      )
    
    return(table_data)
  }, options = list(
    pageLength = 5,
    lengthMenu = c(5, 10, 25, 50),
    searching = TRUE,
    scrollX = TRUE,
    dom = 'tip', # Removes default search box, uses paging and information
    language = list(
      emptyTable = "No HAB advisories found for selected criteria"
    ),
    rowCallback = DT::JS("function(row, data) {
      if(data[1] === 'Active') {
        $('td:eq(1)', row).css('color', 'red').css('font-weight', 'bold');
      }
    }")
  ))
  
  
}

shinyApp(ui, server)