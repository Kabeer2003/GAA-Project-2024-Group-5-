library(shiny)
library(sf)
library(sp)
library(spatstat)
library(tmap)
library(readr)

module1UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Culinary Crossroads"),
    tabPanel("Hawker Centre Distribution",
             h1("1st Order Analysis - Density and Distribution of Hawker Centers"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(ns("region"), "Select Region:", choices = NULL),  # Placeholder for choices
                 uiOutput(ns("location_select")),
                 selectInput(ns("kernel"), "Select Kernel:",
                             choices = list("Gaussian" = "gaussian",
                                            "Epanechnikov" = "epanechnikov",
                                            "Quartic" = "quartic",
                                            "Disc" = "disc"),
                             selected = "gaussian"),
                 sliderInput(ns("bandwidth"), "Select Bandwidth (Sigma):", min = 0.01, max = 12, value = 0.5, step = 0.1)
               ),
               mainPanel(
                 plotOutput(ns("kde_plot"))
               )
             )
    )
  )
}


module1 <- function(input, output, session, mpsz_sf_1, hawker_centre_sp_1) {
  # Define a reactive expression for unique regions
  unique_regions <- reactive({ unique(mpsz_sf_1$REGION_N) })
  
  # Use observe to dynamically update the region selectInput choices
  observe({
    updateSelectInput(session, "region", choices = c("All", unique_regions()))
  })
  # Function to filter data based on region and location selections
  filter_data <- reactive({
    region <- input$region
    location <- input$location
    kernel <- input$kernel
    bandwidth <- input$bandwidth
    
    if(region == "All") {
      sg_owin <- as.owin(mpsz_sf_1)
      hawker_centre_ppp <- as.ppp(hawker_centre_sp_1)
      hawker_centre_SG_ppp <- hawker_centre_ppp[sg_owin] 
      hawker_centre_SG_ppp_km <- rescale(hawker_centre_SG_ppp, 1000, "km")
      return(hawker_centre_SG_ppp_km)
    } else {
      if(is.null(location) || location == "All") {
        selected_region <- mpsz_sf_1[mpsz_sf_1$REGION_N == region,]
        selected_region_owin <- as.owin(selected_region) 
        hawker_centre_ppp <- as.ppp(hawker_centre_sp_1)
        hawker_centre_selected_region_ppp <- hawker_centre_ppp[selected_region_owin] 
        hawker_centre_selected_region_ppp_km <- rescale(hawker_centre_selected_region_ppp, 1000, "km") 
        return(hawker_centre_selected_region_ppp_km)
      } else {
        selected_area <- mpsz_sf_1[mpsz_sf_1$PLN_AREA_N == location,]
        selected_area_owin <- as.owin(selected_area)
        hawker_centre_ppp <- as.ppp(hawker_centre_sp_1)
        hawker_centre_selected_area_ppp <- hawker_centre_ppp[selected_area_owin] 
        hawker_centre_selected_area_ppp_km <- rescale(hawker_centre_selected_area_ppp, 1000, "km") 
        return(hawker_centre_selected_area_ppp_km)
      }
    }
  })
  
  # Dynamic UI for selecting locations based on the region
  output$location_select <- renderUI({
    region <- input$region
    if(region == "All") {
      return(NULL)
    } else {
      locations <- unique(mpsz_sf_1$PLN_AREA_N[mpsz_sf_1$REGION_N == region])
      selectInput(session$ns("location"), "Select Location:", choices = c("All", locations))
    }
  })
  
  # KDE plot output
  output$kde_plot <- renderPlot({
    data <- filter_data()
    if(!is.null(data)) {
      plot(density(data, sigma = input$bandwidth, kernel = input$kernel), main = "Kernel Density Estimation")
    }
  })
}