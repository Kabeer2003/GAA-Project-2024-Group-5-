library(shiny)
library(sf)
library(tmap)
library(dplyr)

# UI definition
module3UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Geographic Accessibility of Hawker Centres in Singapore"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("accessMethod"), "Select Accessibility Scoring Method:",
                    choices = c("Hansen's Method" = "Hansen",
                                "KD2SFCA Method" = "KD2SFCA",
                                "SAM Method" = "SAM")),
        selectInput(ns("regionSelect"), "Choose a Region:", choices = NULL),
        selectInput(ns("planningAreaSelect"), "Choose a Planning Area:", choices = NULL),
        actionButton(ns("goButton"), "Go!")
      ),
      mainPanel(
        tmapOutput(ns("accessMap"))
      )
    )
  )
}

# Assuming the UI definition (module3UI) is unchanged and already defined

# Module3 Server logic
module3 <- function(input, output, session) {
  # Load the full dataset for populating region and planning area choices
  # Ensure the file path is correct relative to the app's running directory
  full_data <- readRDS("data/Module3_Data/RDS/mpsz.rds")
  
  # Update 'Choose a Region' dropdown choices
  observe({
    regions <- sort(unique(full_data$REGION_N))
    updateSelectInput(session, "regionSelect", choices = c("ALL" = "ALL", regions))
  })
  
  # Update 'Choose a Planning Area' dropdown based on selected region
  observeEvent(input$regionSelect, {
    if (input$regionSelect == "ALL") {
      planning_areas <- sort(unique(full_data$PLN_AREA_N))
    } else {
      planning_areas <- full_data %>%
        filter(REGION_N == input$regionSelect) %>%
        .$PLN_AREA_N %>%
        unique() %>%
        sort()
    }
    updateSelectInput(session, "planningAreaSelect", choices = c("ALL" = "ALL", planning_areas))
  })
  
  # Reactive expression to load and filter data based on selected method, region, and planning area
  reactive_data <- reactive({
    dataset <- switch(input$accessMethod,
                      "Hansen" = "hexagon_Hansen_mpsz.rds",
                      "KD2SFCA" = "hexagon_KD2SFCA_mpsz.rds",
                      "SAM" = "hexagon_SAM_mpsz.rds"
    )
    data <- readRDS(paste0("data/Module3_Data/RDS/", dataset))
    
    if (input$regionSelect != "ALL") {
      data <- data %>% filter(REGION_N == input$regionSelect)
    }
    
    if (input$planningAreaSelect != "ALL") {
      data <- data %>% filter(PLN_AREA_N == input$planningAreaSelect)
    }
    
    return(data)
  })
  
  # Define the plot_map function to adjust based on the selected accessibility scoring method
  plot_map <- function(data, method) {
    # Set tmap to plot mode for a static plot
    tmap_mode("plot")
    
    # Define the bounding box from the data
    mapex <- st_bbox(data)
    
    # Determine the column to use for fill based on the selected method
    fill_col <- switch(method,
                       "Hansen" = "accHansen",
                       "KD2SFCA" = "accKD2SFCA",
                       "SAM" = "accSAM")
    
    # Construct the tmap plot
    tm_map <- tm_shape(data, bbox = mapex) +
      tm_fill(col = fill_col, n = 10, style = "quantile",
              border.col = "black", border.lwd = 1, palette = "viridis") +
      tm_layout(main.title = paste("Hawker Centre Accessibility:", method),
                main.title.position = "center", main.title.size = 1,
                legend.outside = FALSE, legend.height = 0.25, legend.width = 0.4,
                legend.format = list(digits = 4), legend.position = c("right", "top"),
                frame = TRUE) +
      tm_scale_bar(width = 0.15) +
      tm_grid(lwd = 0.1, alpha = 0.5)
    
    return(tm_map)  # Return the tmap object for rendering
  }
  
  # Observer for the Go button
  observeEvent(input$goButton, {
    req(input$accessMethod, reactive_data())  # Ensure method and data are available before plotting
    output$accessMap <- renderTmap({
      plot_map(reactive_data(), input$accessMethod)  # Pass the selected method to plot_map
    })
  })
}