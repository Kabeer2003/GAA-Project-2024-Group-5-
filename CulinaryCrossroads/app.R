#DO NOT REMOVE THESE, JUST ADD ON
library(shiny)
library(sf)
library(sp)
library(spatstat)
library(tmap)
library(readr)

# MODULE 1 DATA IMPORT (DO NOT REMOVE OR EDIT)
mpsz_sf_1 <- st_read(dsn = "data/Module1_Data/geospatial", 
                   layer = "MP14_SUBZONE_WEB_PL")

hawker_centre_sf_1 <- read_csv("data/Module1_Data/aspatial/updated_hawker_centres.csv")
hawker_centre_sf_1 <- st_as_sf(hawker_centre_sf_1, coords = c("Longitude", "Latitude"), crs = 4326)

sg_sf_1 <- st_read(dsn = "data/Module1_Data/", 
                 layer = "CostalOutline")

main_island_name <- "SINGAPORE - MAIN ISLAND"
sg_sf_1 <- sg_sf_1[sg_sf_1$COSTAL_NAM == main_island_name, ]

target_crs_1 = st_crs(sg_sf_1)
hawker_centre_sf_1 <- st_transform(hawker_centre_sf_1, target_crs_1)

mpsz_sf_1 <- st_intersection(mpsz_sf_1, sg_sf_1)

hawker_centre_1 <- as_Spatial(hawker_centre_sf_1)
hawker_centre_sp_1 <- as(hawker_centre_1, "SpatialPoints")

# MODULE 2 DATA IMPORT (DO NOT REMOVE OR EDIT)


# MODULE 3 DATA IMPORT (DO NOT REMOVE OR EDIT)



# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Culinary Crossroads"),
  
  #MODULE 1 (DO NOT DELETE OR EDIT)
  tabsetPanel(
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
             h1("2nd Order Analysis - Hawker Centre Proximity"),
             # Add UI elements for page 2
    ),
    #MODULE 3
    tabPanel("Hawker Centre Accessibility",
             h1("Geographic Accessibility Modeling of Hawker Centres"),
             # Add UI elements for page 3
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
  
  
  #MODULE 3 BACKEND CODES 
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
