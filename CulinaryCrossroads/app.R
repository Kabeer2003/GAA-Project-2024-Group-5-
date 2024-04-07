#DO NOT REMOVE THESE, JUST ADD ON
library(shiny)
library(sf)
library(sp)
library(spatstat)
library(tmap)
library(readr)
getwd()

# MODULE 1 DATA IMPORT (DO NOT REMOVE OR EDIT)
data_dir <- "data/Module1_Data/rds"
mpsz_sf_1 <- readRDS(file.path(data_dir, "mpsz.rds"))

hawker_centre_sf_1 <- read.csv("data/Module1_Data/aspatial/updated_hawker_centres.csv")
hawker_centre_sf_1 <- st_as_sf(hawker_centre_sf_1, coords = c("Longitude", "Latitude"), crs = 4326)

sg_sf_1 <- readRDS(file.path(data_dir, "sg.rds"))

main_island_name <- "SINGAPORE - MAIN ISLAND"
sg_sf_1 <- sg_sf_1[sg_sf_1$COSTAL_NAM == main_island_name, ]

target_crs_1 = st_crs(sg_sf_1)
hawker_centre_sf_1 <- st_transform(hawker_centre_sf_1, target_crs_1)

mpsz_sf_1 <- st_intersection(mpsz_sf_1, sg_sf_1)

hawker_centre_1 <- as_Spatial(hawker_centre_sf_1)
hawker_centre_sp_1 <- as(hawker_centre_1, "SpatialPoints")

# MODULE 2 DATA IMPORT (DO NOT REMOVE OR EDIT)
data_dir <- "data/Module2_Data"
singapore_mpsz2 <- readRDS(file.path(data_dir, "singapore_mpsz2.rds"))
hawker_stalls_data2 <- readRDS(file.path(data_dir, "hawker_centre_pts2.rds"))

# Helper functions
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

find_significance_level <- function(numSim) {
  return(2 / (1 + numSim))
}

# MODULE 3 DATA IMPORT (DO NOT REMOVE OR EDIT)
data_dir <- "data/Module3_Data"
full_data <- readRDS(file.path(data_dir, "/RDS/mpsz.rds"))

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Culinary Crossroads"),
  
  tabsetPanel(
    #MODULE 1 (DO NOT DELETE OR EDIT)
    tabPanel("Hawker Centre Distribution",
             h1("1st Order Analysis - Density and Distribution of Hawker Centers"),
             # Add UI elements for page 1
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("region", "Select Region:", choices = c("All", unique(mpsz_sf_1$REGION_N))),
                 uiOutput("location_select"),
                 selectInput(inputId = "kernel",
                             label = "Select Kernel:",
                             choices = list("Gaussian" = "gaussian",
                                            "Epanechnikov" = "epanechnikov",
                                            "Quartic" = "quartic",
                                            "Disc" = "disc"),
                             selected = "gaussian"),
                 sliderInput("bandwidth", "Select Bandwidth (Sigma):", min = 0.01, max = 12, value = 0.5, step = 0.1)
               ),
               mainPanel(
                 plotOutput("kde_plot")
               )
             )
    ),
    #MODULE 2
    tabPanel("Hawker Centre Proximity",
             h1("Second Order Spatial Point Analysis"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("region2", "Select Region:", choices = c("All", unique(singapore_mpsz2$REGION_N))),
                 selectInput("planningArea2", "Select Planning Area:", choices = c("All", unique(singapore_mpsz2$PLN_AREA_N))),
                 textInput("dishName2", "Enter Dish Name:"),
                 numericInput("numSim2", "Number of Simulations:", value = 39, min = 1),
                 selectInput("testType2", "Select Test Type:", 
                             choices = c("F Function" = "F", "G Function" = "G", 
                                         "K Function" = "K", "L Function" = "L")),
                 actionButton("goButton2", "Analyze"),
                 textOutput("sigLevel2"),
                 textOutput("dataPointCount2")
               ),
               mainPanel(
                 fluidRow(
                   column(7, plotOutput("testOutput2")),
                   column(5, plotOutput("mapOutput2"))
                 )
               )
             )
    ),
    #MODULE 3
    tabPanel("Hawker Centre Accessibility",
             h1("Geographic Accessibility Modeling of Hawker Centres"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("accessMethod", "Select Accessibility Scoring Method:",
                             choices = c("Hansen's Method" = "Hansen",
                                         "KD2SFCA Method" = "KD2SFCA",
                                         "SAM Method" = "SAM")),
                 selectInput("regionSelect", "Choose a Region:", choices = NULL),
                 selectInput("planningAreaSelect", "Choose a Planning Area:", choices = NULL),
                 actionButton("goButton", "Go!")
               ),
               mainPanel(
                 tmapOutput("accessMap")
               )
             )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  #MODULE 1 BACKEND CODES (DO NOT DELETE OR EDIT)
  # Function to filter data based on region and location selections
  filter_data <- reactive({
    region <- input$region
    location <- input$location
    kernel <- input$kernel
    bandwidth <- input$bandwidth
    
    # Your data filtering and processing logic here
    if(region == "All") {
      sg_owin_1 <- as.owin(mpsz_sf_1)
      hawker_centre_ppp_1 <- as.ppp(hawker_centre_sf_1)
      hawker_centre_SG_ppp_1 <- hawker_centre_ppp_1[sg_owin_1] 
      hawker_centre_SG_ppp_km_1 <- rescale(hawker_centre_SG_ppp_1, 1000, "km")
      return(hawker_centre_SG_ppp_km_1)
    } else {
      if(is.null(location) || location == "All") {
        selected_region_1 <- mpsz_sf_1[mpsz_sf_1$REGION_N == region,]
        selected_region_owin_1 <- as.owin(selected_region_1) 
        hawker_centre_ppp_1 <- as.ppp(hawker_centre_sf_1)
        hawker_centre_selected_region_ppp_1 <- hawker_centre_ppp_1[selected_region_owin_1] 
        hawker_centre_selected_region_ppp_km_1 <- rescale(hawker_centre_selected_region_ppp_1, 1000, "km") 
        return(hawker_centre_selected_region_ppp_km_1)
      } else {
        selected_area_1 <- mpsz_sf_1[mpsz_sf_1$PLN_AREA_N == location,]
        selected_area_owin_1 <- as.owin(selected_area_1)
        hawker_centre_ppp_1 <- as.ppp(hawker_centre_sf_1)
        hawker_centre_selected_area_ppp_1 <- hawker_centre_ppp_1[selected_area_owin_1] 
        hawker_centre_selected_area_ppp_km_1 <- rescale(hawker_centre_selected_area_ppp_1, 1000, "km") 
        return(hawker_centre_selected_area_ppp_km_1)
      }
    }
  })
  
  # Output for dynamic location select input
  output$location_select <- renderUI({
    region <- input$region
    if(region == "All") {
      return(NULL)
    } else {
      locations <- unique(mpsz_sf_1$PLN_AREA_N[mpsz_sf_1$REGION_N == region])
      selectInput("location", "Select Location:", choices = c("All", locations))
    }
  })
  
  # Output for KDE plot
  output$kde_plot <- renderPlot({
    data <- filter_data()
    if(!is.null(data)) {
      plot(density(data, sigma = input$bandwidth, kernel = input$kernel), main = "Kernel Density Estimation")
    }
  })
  
  #MODULE 2 BACKEND CODES 
  observeEvent(input$goButton2, {
    # Retrieve inputs
    print("Button clicked, retrieving inputs...")
    region2 <- input$region2
    planningArea2 <- input$planningArea2
    dishName2 <- input$dishName2
    numSim2 <- input$numSim2
    testType2 <- input$testType2
    
    # Filter map and hawkers
    print("Filtering map and hawkers...")
    filtered_map <- if(region2 == "All") singapore_mpsz2 else singapore_mpsz2[singapore_mpsz2$REGION_N == region2, ]
    
    if (planningArea2 != "All") {
      filtered_map <- filtered_map[filtered_map$PLN_AREA_N == planningArea2, ]
    }
    
    filtered_hawkers <- if (dishName2 != "") {
      hawker_stalls_data2[grep(dishName2, hawker_stalls_data2$`Hawker Centre Stalls`, ignore.case = TRUE), ]
    } else {
      hawker_stalls_data2
    }
    
    # Print debugging information
    print("Debugging filtered_map...")
    print(filtered_map)
    
    # Check the dimensions of filtered_map and filtered_hawkers
    print("Dimensions of filtered_map:")
    print(dim(filtered_map))
    
    print("Dimensions of filtered_hawkers:")
    print(dim(filtered_hawkers))
    
    # Convert to spatstat object
    print("Converting to spatstat object...")
    
    filtered_map_owin_1 <- as.owin(filtered_map)
    filtered_hawkers_ppp_1 <- as.ppp(filtered_hawkers)
    clipped_ppp <- filtered_hawkers_ppp_1[filtered_map_owin_1] 
    
    print("Conversion to spatstat object completed.")
    
    output$dataPointCount2 <- renderText({
      sprintf("Number of Hawkers in Given Area: %d", clipped_ppp$n)
    })
    
    output$mapOutput2 <- renderPlot({
      plot(clipped_ppp$window, main = "Map with Hawkers")
      plot(clipped_ppp, add = TRUE, pch = 20, col = 'red')
    })
    
    output$testOutput2 <- renderPlot({
      plot_envelope(clipped_ppp, testType2, numSim2)
    })
    
    output$sigLevel2 <- renderText({
      sprintf("Significance Level: %.4f", find_significance_level(numSim2))
    })
  })
  
  # MODULE 3 BACKEND CODES
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
    
    tm_map  # Return the tmap object for rendering
  }
  
  # Observer for the Go button
  observeEvent(input$goButton, {
    req(input$accessMethod)  # Ensure accessMethod is available before proceeding
    req(input$regionSelect, input$planningAreaSelect)  # Ensure region and planning area are selected
    # Load data based on access method
    reactive_data <- reactive({
      input_access_method <- input$accessMethod  # Store the value of input$accessMethod
      
      dataset <- switch(input_access_method,
                        "Hansen" = "hexagon_Hansen_mpsz.rds",
                        "KD2SFCA" = "hexagon_KD2SFCA_mpsz.rds",
                        "SAM" = "hexagon_SAM_mpsz.rds")
      
      readRDS(file.path(data_dir, "RDS", dataset))
    })
    req(reactive_data())  # Ensure data is available before plotting
    # Filter data based on selected region and planning area
    filtered_data <- reactive({
      data <- reactive_data()
      region <- input$regionSelect
      planning_area <- input$planningAreaSelect
      
      if (region != "ALL") {
        data <- data[data$REGION_N == region, ]
      }
      if (planning_area != "ALL") {
        data <- data[data$PLN_AREA_N == planning_area, ]
      }
      
      return(data)
    })
    
    # Render the map based on filtered data
    output$accessMap <- renderTmap({
      plot_map(filtered_data(), input$accessMethod)  # Pass the selected method to plot_map
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
