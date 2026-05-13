library(shiny)
library(tidyverse)
library(ggplot2)
library(dataRetrieval)

# Define UI 
ui <- fluidPage(

# Application title
titlePanel("Nooksack River Profile"),

# Sidebar layout
sidebarLayout(
  
  # Sidebar panel
  sidebarPanel(
    
    checkboxGroupInput(
      inputId = "my_checkbox_group",
      label = "Choose your stream gauges:",
      choices = list(
        "USGS-12213100 (Ferndale)" = "A",
        "USGS-12211200 (Everson)" = "B",
        "USGS-12210700 (North Cedarville)" = "C",
        "USGS-12210000 (South Fork)" = "D",
        "USGS-1220800 (Middle Fork)" = "E",
        "USGS-12205000 (North Fork)" = "F"
      ),
      selected = "A"
    ),
    
    selectInput(
      inputId = "variable",
      label = "Choose a variable to plot:",
      choices = c(
        "Gauge Height" = "height",
        "Discharge" = "Q",
        "Turbidity" = "turb"
      )
    ),
    
    dateRangeInput(
      inputId = "date_range",
      label = "Select Date Range:",
      start = Sys.Date() - 7,
      end = Sys.Date()
    )
    
    # textOutput("selected_range")
    
  ),
  
  # Main panel
  mainPanel(
    plotOutput("my_plot")
  )
  
)
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
