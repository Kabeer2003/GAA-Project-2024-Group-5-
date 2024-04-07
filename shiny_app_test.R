library(shiny)
library(sf)
library(sp)
library(spatstat)
library(dplyr)
library(readr)
library(stringr)

# Set the directory path to your data
data_dir <- "D:/Kabeer2003/GAA Project Group 5/GAA-Project-2024-Group-5-/Module2/data/"

# UI definition
ui <- fluidPage(
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
      selectInput("region", "Select Region:", choices = NULL),
      selectInput("planningArea", "Select Planning Area:", choices = c("All" = "All")),
      textInput("dishName", "Enter Dish Name:"),
      numericInput("numSim", "Number of Simulations:", value = 39, min = 1),
      selectInput("testType", "Select Test Type:", 
                  choices = c("F Function" = "F", "G Function" = "G", 
                              "K Function" = "K", "L Function" = "L")),
      actionButton("goButton", "Analyze"),
      textOutput("sigLevel"),
      textOutput("dataPointCount")
    ),
    mainPanel(
      fluidRow(
        column(7, plotOutput("testOutput")),
        column(5, plotOutput("mapOutput"))
      )
    )
  )
)

# Conversion function to spatstat objects
convert_to_spatstat <- function(st_polygon_df, st_points_df) {
  owin <- st_polygon_df %>%
    st_geometry() %>%
    as_Spatial() %>%
    as("owin")
  
  ppp <- st_points_df %>%
    st_geometry() %>%
    as_Spatial() %>%
    as("ppp")
  
  clipped_ppp <- ppp[owin]
  return(clipped_ppp)
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

# Server logic
server <- function(input, output, session) {
  singapore_mpsz2 <- readRDS(file.path(data_dir, "singapore_mpsz2.rds"))
  hawker_stalls_data2 <- readRDS(file.path(data_dir, "hawker_centre_pts2.rds"))
  
  observe({
    regions <- unique(singapore_mpsz2$REGION_N)
    updateSelectInput(session, "region", choices = c("All" = "All", sort(regions)))
  })
  
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
  
  output$sigLevel <- renderText({
    sprintf("Significance Level: %.4f", find_significance_level(input$numSim))
  })
}

shinyApp(ui, server)
