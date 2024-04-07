library(shiny)
library(sf)
library(sp)
library(spatstat)
library(dplyr)
library(readr)
library(stringr)

# Set the directory path to your data
data_dir <- "data/Module2_Data"

module2UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(
        HTML("
        .title-wrapper {
          text-align: center;
        }
        ")
      )
    ),
    titlePanel(
      div(class = "title-wrapper", "Second Order Spatial Point Analysis")
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("region"), "Select Region:", choices = NULL),
        selectInput(ns("planningArea"), "Select Planning Area:", choices = c("All" = "All")),
        textInput(ns("dishName"), "Enter Dish Name:"),
        numericInput(ns("numSim"), "Number of Simulations:", value = 39, min = 1),
        selectInput(ns("testType"), "Select Test Type:", 
                    choices = c("F Function" = "F", "G Function" = "G", 
                                "K Function" = "K", "L Function" = "L")),
        actionButton(ns("goButton"), "Analyze"),
        textOutput(ns("sigLevel")),
        textOutput(ns("dataPointCount"))
      ),
      mainPanel(
        fluidRow(
          column(7, plotOutput(ns("testOutput"))),
          column(5, plotOutput(ns("mapOutput")))
        )
      )
    )
  )
}


module2 <- function(input, output, session) {
  # Function to convert data to spatstat objects
  convert_to_spatstat <- function(st_polygon_df, st_points_df) {
    # Ensure that st_polygon_df is a SpatialPolygonsDataFrame
    if (!inherits(st_polygon_df, "SpatialPolygonsDataFrame")) {
      st_polygon_df <- as(st_polygon_df, "SpatialPolygonsDataFrame")
    }
    
    # Convert SpatialPolygonsDataFrame to owin
    polys <- slot(st_polygon_df, "polygons")
    if (length(polys) > 1) {
      polys <- do.call(rbind, lapply(polys, function(x) slot(x, "Polygons")[[1]]@coords))
    } else {
      polys <- polys[[1]]@Polygons[[1]]@coords
    }
    owin <- spatstat::owin(poly = list(x = polys[, 1], y = polys[, 2]))
    
    # Ensure that st_points_df is a SpatialPointsDataFrame
    if (!inherits(st_points_df, "SpatialPointsDataFrame")) {
      st_points_df <- as(st_points_df, "SpatialPointsDataFrame")
    }
    
    # Convert SpatialPointsDataFrame to ppp
    coords <- coordinates(st_points_df)
    ppp <- spatstat::ppp(x = coords[, 1], y = coords[, 2], window = owin)
    
    return(ppp)
  }
  
  # Function to perform the test and plot the results with envelopes
  plot_envelope <- function(ppp, test_type, num_sim) {
    test_func <- switch(
      test_type,
      "F" = Fest,
      "G" = Gest,
      "K" = Kest,
      "L" = Lest,
      stop("Unknown test type selected")
    )
    
    env_result <- envelope(ppp, fun = test_func, nsim = num_sim, verbose = FALSE)
    
    plot(env_result, main = paste(test_type, "Function Test"))
    
    if ("Hi" %in% names(env_result) && "Lo" %in% names(env_result)) {
      r_values <- env_result$r
      hi <- env_result$hi
      lo <- env_result$lo
      xvals <- c(r_values, rev(r_values))
      yvals <- c(hi, rev(lo))
      polygon(xvals, yvals, col = rgb(0.8, 0.8, 0.8, 0.5), border = NA)
    }
  }
  
  # Function to calculate the significance level
  find_significance_level <- function(numSim) {
    2 / (1 + numSim)
  }
  
  # Load data
  singapore_mpsz2 <- readRDS(file.path(data_dir, "singapore_mpsz2.rds"))
  hawker_stalls_data2 <- readRDS(file.path(data_dir, "hawker_centre_pts2.rds"))
  
  # Update 'Select Region' dropdown choices
  observe({
    regions <- unique(singapore_mpsz2$REGION_N)
    updateSelectInput(session, "region", choices = c("All" = "All", sort(regions)))
  })
  
  # Update 'Select Planning Area' dropdown based on selected region
  observeEvent(input$region, {
    planning_areas <- if (input$region == "All") {
      c("All")
    } else {
      singapore_mpsz2 %>%
        dplyr::filter(REGION_N == input$region) %>%
        dplyr::pull(PLN_AREA_N) %>%
        unique() %>%
        sort()
    }
    updateSelectInput(session, "planningArea", choices = c("All" = "All", planning_areas))
  })
  
  # Perform analysis and plot results when 'Analyze' button is clicked
  observeEvent(input$goButton, {
    req(input$region)
    
    filtered_map <- singapore_mpsz2
    if (input$region != "All") {
      filtered_map <- filtered_map %>% dplyr::filter(REGION_N == input$region)
    }
    if (input$planningArea != "All") {
      filtered_map <- filtered_map %>% dplyr::filter(PLN_AREA_N == input$planningArea)
    }
    
    filtered_hawkers <- hawker_stalls_data2
    if (input$dishName != "") {
      filtered_hawkers <- filtered_hawkers %>%
        dplyr::filter(str_detect(`Hawker Centre Stalls`, regex(input$dishName, ignore_case = TRUE)))
    }
    
    clipped_ppp <- convert_to_spatstat(filtered_map, filtered_hawkers)
    
    output$dataPointCount <- renderText({
      sprintf("Number of Hawkers in Given Area: %d", clipped_ppp$n)
    })
    
    output$mapOutput <- renderPlot({
      plot(clipped_ppp$window, main = "Map with Hawkers")
      plot(clipped_ppp, add = TRUE, pch = 20, col = 'red')
    })
    
    output$testOutput <- renderPlot({
      plot_envelope(clipped_ppp, input$testType, input$numSim)
    })
  })
  
  # Display the significance level
  output$sigLevel <- renderText({
    sprintf("Significance Level: %.4f", find_significance_level(input$numSim))
  })
}