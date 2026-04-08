library(shiny)

source("modules/visualization.R")
source("modules/mapping.R")
source("modules/prediction.R")

server <- function(input, output) {
  
  filtered_data <- reactive({
    cleaned_data %>%
      filter(
        Disease %in% input$disease,
        State %in% input$state,
        Year >= input$yearRange[1],
        Year <= input$yearRange[2]
      )
  })
  
  # KPI
  # KPI CARDS
  
  output$casesBox <- renderUI({
    div(class = "custom-box cases-box",
        
        icon("virus", style = "font-size: 30px;"),
        br(),
        
        format(sum(filtered_data()$Cases, na.rm = TRUE), big.mark = ","),
        br(),
        span("Total Cases", style = "font-size: 16px;")
    )
  })
  
  output$deathsBox <- renderUI({
    div(class = "custom-box deaths-box",
        
        icon("skull-crossbones", style = "font-size: 30px;"),
        br(),
        
        format(sum(filtered_data()$Deaths, na.rm = TRUE), big.mark = ","),
        br(),
        span("Total Deaths", style = "font-size: 16px;")
    )
  })
  
  output$activeBox <- renderUI({
    div(class = "custom-box active-box",
        
        icon("heartbeat", style = "font-size: 30px;"),
        br(),
        
        format(sum(filtered_data()$Cases - filtered_data()$Deaths, na.rm = TRUE), big.mark = ","),
        br(),
        span("Estimated Active", style = "font-size: 16px;")
    )
  })
  output$totalCases <- renderText({
    format(sum(filtered_data()$Cases, na.rm = TRUE), big.mark = ",")
  })
  
  output$totalDeaths <- renderText({
    format(sum(filtered_data()$Deaths, na.rm = TRUE), big.mark = ",")
  })
  # STATE-WISE SUMMARY TABLE
  output$stateSummaryTable <- renderTable({
    
    data <- filtered_data()
    
    summary <- data %>%
      group_by(State) %>%
      summarise(
        Total_Cases = sum(Cases, na.rm = TRUE),
        Total_Deaths = sum(Deaths, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Add GRAND TOTAL row
    total_row <- data.frame(
      State = "TOTAL",
      Total_Cases = sum(summary$Total_Cases),
      Total_Deaths = sum(summary$Total_Deaths)
    )
    
    final_table <- bind_rows(summary, total_row)
    
    return(final_table)
  })
  # DISEASE-WISE SUMMARY TABLE
  output$diseaseSummaryTable <- renderTable({
    
    data <- filtered_data()
    
    summary <- data %>%
      group_by(Disease) %>%
      summarise(
        Total_Cases = sum(Cases, na.rm = TRUE),
        Total_Deaths = sum(Deaths, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Add TOTAL row
    total_row <- data.frame(
      Disease = "TOTAL",
      Total_Cases = sum(summary$Total_Cases),
      Total_Deaths = sum(summary$Total_Deaths)
    )
    
    final_table <- bind_rows(summary, total_row)
    
    return(final_table)
  })
  # INTERACTIVE GRAPHS
  output$casesPlot <- renderPlotly({
    plot_cases(filtered_data())
  })
  
  output$deathsPlot <- renderPlotly({
    plot_deaths(filtered_data())
  })
  
  # STATE ANALYSIS (TOP 5 HIGHLIGHT)
  output$stateMap <- renderPlotly({
    
    state_data <- state_analysis(filtered_data())
    
    plot_ly(state_data,
            x = ~total_cases,
            y = ~reorder(State, total_cases),
            type = 'bar',
            orientation = 'h',
            color = ~Highlight,
            colors = c("Top 5" = "red", "Others" = "lightblue"),
            text = ~paste("Death Rate:", round(death_rate,2)),
            hoverinfo = "text")
  })
  
  # COMPARISON GRAPH
  output$comparePlot <- renderPlotly({
    
    comp_data <- filtered_data() %>%
      group_by(State, Disease) %>%
      summarise(total_cases = sum(Cases), .groups = "drop")
    
    plot_ly(comp_data,
            x = ~State,
            y = ~total_cases,
            color = ~Disease,
            type = 'bar')
  })
  
  # PREDICTION
  output$predTable <- renderTable({
    predict_future(filtered_data())
  })
}