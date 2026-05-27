library(shiny)
library(tidyverse)
library(ggplot2)
library(dataRetrieval)
library(plotly)

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
        "USGS-12210000 (South Fork, Acme)" = "USGS-12210000",
        "USGS-12208000 (Middle Fork, near Deming)" = "USGS-12208000",
        "USGS-12205000 (North Fork, Glacier)" = "USGS-12205000"
      ),
      selected = "USGS-12213100"
    ),
    

    selectInput(
      inputId = "parameter",
      label = "Choose a variable to plot:",
      choices = c(
        "Gauge Height" = "00065",
        "Discharge" = "00060"
      )
    ),
   
  sliderInput("date_range",
              "Select water year range:",
              min = 1991,
              max = 2026,
              value=c(1991, 2026),
              step = 1,
              sep = ""
),

  # aggregation checkbox
  checkboxInput(
    inputId = "use_aggregation",
    label = "Aggregate data",
    value = FALSE
  ),

  conditionalPanel(
    condition = "input.use_aggregation == true",
    
    selectInput(
      inputId = "aggregation",
      label = "Aggregation interval:",
      choices = c(
        "Weekly" = "week",
        "Monthly" = "month",
        "Yearly" = "year"
      ),
      selected = "week"
    )
  )
    
  ),
  
  # Main panel
  mainPanel(
    plotlyOutput("my_plot"),
    
    tableOutput("my_table")
    )
  )
)


server <- function(input, output) {
  # reactive to get the raw data
  water_data <- reactive({
    req(input$gauges, input$date_range, input$parameter) # inputs
    
    # convert water years to dates
    start_date <- as.Date(
      paste0(input$date_range[1] - 1, "-10-01")
    )
    
    end_date <- as.Date(
      paste0(input$date_range[2], "-09-30")
    )
    
    # daily values retrieval
    read_waterdata_daily(
      monitoring_location_id = input$gauges,
      parameter_code = input$parameter,
      time = c(start_date, end_date),
    )
  })
  
  # aggregate the data if requested
  plot_data <- reactive({
    
    df <- water_data()
    
    # no aggregation
    if (!input$use_aggregation) {
      
      df$period <- df$time
      return(df)
      
    }
    
    
    # aggregate data
    df %>%
      mutate(
        period = floor_date(time, unit = input$aggregation)
      ) %>%
      group_by(monitoring_location_id, period) %>%
      summarize(
        value = mean(value, na.rm = TRUE),
        .groups = "drop"
      )
    
  })  
  
  # axis labels
  parameter_labels <- c(
    "00060" = "Discharge (cfs)",
    "00065" = "Gauge Height (ft)"
  )
  
  # Plot
  output$my_plot <- renderPlotly({
    
    df <- plot_data()
    
    ggplot(
      df,
      aes(
        x = period,
        y = value,
        color = monitoring_location_id
      )
    ) +
      geom_line(linewidth = 1) +
      geom_smooth(method = "loess") +
      labs(
        x = "Date",
        y = parameter_labels[input$parameter],
        color = "Gauge"
      ) +
      scale_color_viridis_d() +
      theme_bw()
    
  })
  
  # Summary table
  
  output$parameter <- renderTable(count_top(selected(), diag), width = "100%",
                                  caption = paste("Diagnosis"))
}



# Run the application 
shinyApp(ui = ui, server = server)
