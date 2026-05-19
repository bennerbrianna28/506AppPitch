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
      inputId = "gauges",
      label = "Choose your stream gauges:",
      choices = list(
        "USGS-12213100 (Ferndale)" = "USGS-12213100",
        "USGS-12211200 (Everson)" = "USGS-12211200",
        "USGS-12210700 (North Cedarville)" = "USGS-12210700",
        "USGS-12210000 (South Fork)" = "USGS-12210000",
        "USGS-12208000 (Middle Fork)" = "USGS-12208000",
        "USGS-12205000 (North Fork)" = "USGS-12205000"
      ),
      selected = "USGS-12213100"
    ),
    
    selectInput(
      inputId = "parameter",
      label = "Choose a variable to plot:",
      choices = c(
        "Gauge Height" = "00065",
        "Discharge" = "00060",
        "Turbidity" = "63680"
      )
    ),
    
    #dateRangeInput(
      #inputId = "date_range",
      #label = "Date Range:",
      #start = "1990-10-01",
      #end = Sys.Date()
    #)
  
  sliderInput("dateSlider",
              "Dates:",
              min = as.Date("1990-10-01","%Y-%m-%d"),
              max = as.Date("2026-09-30","%Y-%m-%d"),
              value=as.Date("1990-10-01"),
              timeFormat="%Y-%m-%d")
    
    
  ),
  
  # Main panel
  mainPanel(
    plotOutput("my_plot"),
    )
  )
)

# Define server logic required to draw a histogram

# to revert back to the dateRangeInput, replace dateSlider with date_range
server <- function(input, output) {
  # reactive to get the data
  water_data <- reactive({
    req(input$gauges, input$dateSlider, input$parameter) # inputs
    
    # daily values retrieval
    read_waterdata_daily(
      monitoring_location_id = input$gauges,
      parameter_code = input$parameter,
      time = c(input$dateSlider[1], input$dateSlider[2]),
    )
  })
  
  parameter_labels <- c(
    "00060" = "Discharge (cfs)",
    "00065" = "Gauge Height (ft)",
    "63680" = "Turbidity (NTU)"
  )
  
  # Plot
  output$my_plot <- renderPlot({
    
    df <- water_data()
    
    # identify value column
    value_col <- "value"
    
    ggplot(
      df,
      aes(
        x = time,
        y = .data[[value_col]],
        color = monitoring_location_id
      )
    ) +
      geom_line(linewidth = 1) +
      labs(
        x = "Date",
        y = parameter_labels[input$parameter],
        color = "Gauge"
      ) +
      theme_bw()
    
  })
    
}

  


# Run the application 
shinyApp(ui = ui, server = server)
