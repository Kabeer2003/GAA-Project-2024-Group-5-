library(shiny)

source("TEST_MOD3.R")
source("TEST_MOD2.R")

# Master App UI
ui <- fluidPage(
  titlePanel("Master Shiny App for Hawker Centres"),
  tabsetPanel(
    tabPanel("Module 2", module2UI("module2")),
    tabPanel("Module 3", module3UI("module3")),
    selected = "Module 2"  # Set the default tab to Module 3
  )
)

# Master App Server
server <- function(input, output, session) {
  callModule(module2, "module2")
  callModule(module3, "module3")
}

# Run the App
shinyApp(ui = ui, server = server)
