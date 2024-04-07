library(shiny)

source("TEST_MOD3.R")  # Ensure this file defines module3UI and module3 functions

# Master App UI
ui <- fluidPage(
  titlePanel("Master Shiny App for Hawker Centres"),
  tabsetPanel(
    # Assuming the other module UIs are commented out for testing
    tabPanel("Module 3", module3UI("module3")),  # Ensure module3UI is defined in TEST_MOD3.R or elsewhere
    selected = "Module 3"  # Set the default tab to Module 3
  )
)

# Master App Server
server <- function(input, output, session) {
  callModule(module3, "module3")  # Ensure the module3 function is defined and correctly sourced
}

# Run the App
shinyApp(ui = ui, server = server)
