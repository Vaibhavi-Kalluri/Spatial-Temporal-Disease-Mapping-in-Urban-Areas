library(shiny)

source("modules/visualization.R")
source("modules/mapping.R")
source("modules/prediction.R")

server <- function(input, output) {
  
  filtered_data <- reactive({
    cleaned_data %>%
      filter(
        Disease == input$disease,
        State == input$state,
        Year >= input$yearRange[1],
        Year <= input$yearRange[2]
      )
  })
  
  output$totalCases <- renderText({
    sum(filtered_data()$Cases, na.rm = TRUE)
  })
  
  output$totalDeaths <- renderText({
    sum(filtered_data()$Deaths, na.rm = TRUE)
  })
  
  output$casesPlot <- renderPlot({
    plot_cases(filtered_data())
  })
  
  output$deathsPlot <- renderPlot({
    plot_deaths(filtered_data())
  })
  
  output$predTable <- renderTable({
    predict_future(filtered_data())
  })
}