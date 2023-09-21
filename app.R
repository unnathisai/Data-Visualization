library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Weather Dashboard"),
  
  dashboardSidebar(
    selectInput("city", "City", choices = c("Bengaluru", "Bombay", "Delhi", "Hyderabad", "Jaipur", "Kanpur", "Nagpur", "Pune"), selected = "Delhi"),
    selectInput("xcol", "X Axis", choices = c("maxtempC", "mintempC", "totalSnow_cm", "sunHour", "DewPointC", "FeelsLikeC", "HeatIndexC", "WindChillC", "cloudcover", "humidity", "pressure", "tempC", "visibility", "windspeedKmph"), selected = "maxtempC"),
    selectInput("ycol", "Y Axis", choices = c("maxtempC", "mintempC", "totalSnow_cm", "sunHour", "DewPointC", "FeelsLikeC", "HeatIndexC", "WindChillC", "cloudcover", "humidity", "pressure", "tempC", "visibility", "windspeedKmph"), selected = "mintempC"),
    numericInput("bins", "Number of bins:", value = 30)
  ),
  
  dashboardBody(
    fluidRow(
      box(title = "Scatterplot", plotOutput("scatterplot", height = 325, hover = "plot_hover")),
      box(title = "Histogram", plotOutput("histogram", height = 325, hover = "plot_hover")),
      box(title = "Density Plot", plotOutput("densityplot", height = 325, hover = "plot_hover")),
      box(title = "Bubble Chart", plotOutput("bubblechart", height = 325, hover = "plot_hover")),
      box(title = "Heatmap", plotOutput("heatmap", height = 325, hover = "plot_hover")),
      box(title = "Box Plot", plotOutput("boxplot", height = 325, hover = "plot_hover")),
      box(title = "Area Chart", plotOutput("areachart", height = 325, hover = "plot_hover")),
      box(title = "Line Plot", plotOutput("lineplot", height = 325, hover = "plot_hover"))
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Load data
  city_data <- function() {
    city <- switch(input$city, 
                   "Bengaluru" = "bengaluru.csv",
                   "Bombay" = "bombay.csv",
                   "Delhi" = "delhi.csv",
                   "Hyderabad" = "hyderabad.csv",
                   "Jaipur" = "jaipur.csv",
                   "Kanpur" = "kanpur.csv",
                   "Nagpur" = "nagpur.csv",
                   "Pune" = "pune.csv")
    read.csv(city)
  }
  
  # Scatterplot
  output$scatterplot <- renderPlot({
    ggplot(city_data(), aes(x = get(input$xcol), y = get(input$ycol))) +
      geom_point(color = "purple") +
      labs(x = input$xcol, y = input$ycol)
  })
  
  # Histogram
  output$histogram <- renderPlot({
    ggplot(city_data(), aes(x = get(input$xcol))) +
      geom_histogram(binwidth = 1, fill = "purple", color = "white", bins = input$bins) +
      labs(x = input$xcol, y = "Frequency")
  })
  
  # Density plot
  output$densityplot <- renderPlot({
    ggplot(city_data(), aes(x = get(input$xcol))) +
      geom_density(fill = "purple", color = "white",  alpha = 0.5) +
      labs(x = input$xcol, y = "Density")
  })
  
  # Bubble chart
  output$bubblechart <- renderPlot({
    ggplot(city_data(), aes(x = get(input$xcol), y = get(input$ycol), size = maxtempC)) +
      geom_point(aes(color = maxtempC), alpha = 0.1) +
      scale_size_continuous(range = c(1,3)) +
      scale_color_gradient(low = "pink", high = "purple") +
      labs(x = input$xcol, y = input$ycol, size = "Population", color = "Max Temp") +
      theme_bw()
  })
  
  # Heatmap
  output$heatmap <- renderPlot({
    ggplot(city_data(), aes(x = get(input$xcol), y = get(input$ycol))) +
      geom_tile(aes(fill = maxtempC), color = "white") +
      scale_fill_gradient(low = "pink", high = "purple") +
      labs(x = input$xcol, y = input$ycol, fill = input$xcol)
  })
  
  # Box plot
  output$boxplot <- renderPlot({
    ggplot(city_data(), aes(x = get(input$xcol), y = get(input$ycol))) +
      geom_boxplot(fill = "purple", color = "purple") +
      labs(x = input$xcol, y = input$ycol)
  })
  
  # Line plot
  output$lineplot <- renderPlot({
    ggplot(city_data(), aes(x = get(input$xcol), y = get(input$ycol))) +
      geom_line(color = "purple") +
      labs(x = input$xcol, y = input$ycol)
  })
  
  # Area Chart
  output$areachart <- renderPlot({
    ggplot(city_data(), aes(x = get(input$xcol), y = get(input$ycol))) +
      geom_area(fill = "purple", color = "white") +
      labs(x = input$xcol, y = input$ycol)
  })

}

# Run the application
shinyApp(ui = ui, server = server)