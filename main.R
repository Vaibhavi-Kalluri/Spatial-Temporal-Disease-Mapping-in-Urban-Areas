# 🔹 Install packages if needed
# install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "lubridate"))

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)

# 🔹 LOAD DATA (MAKE SURE FILE NAME IS CORRECT)
data <- read.csv("C:/Users/dp765/OneDrive/Desktop/r data set/covid_19_india.csv")

# 🔹 RENAME COLUMNS
colnames(data) <- c("sno", "date", "time", "state",
                    "indian", "foreign", "cured", "deaths", "confirmed")

# 🔹 CONVERT DATE
data$date <- as.Date(data$date)

# 🔹 UI
ui <- dashboardPage(
  
  dashboardHeader(title = "COVID Spatial-Temporal Dashboard (India)"),
  
  dashboardSidebar(
    
    dateRangeInput("date_range", "Select Date Range:",
                   start = min(data$date),
                   end = max(data$date)),
    
    selectInput("state", "Select State:",
                choices = unique(data$state),
                selected = "Kerala")
  ),
  
  dashboardBody(
    
    # 🔹 KPI CARDS
    fluidRow(
      valueBoxOutput("total_cases"),
      valueBoxOutput("total_deaths"),
      valueBoxOutput("total_cured")
    ),
    
    # 🔹 PLOTS
    fluidRow(
      box(title = "Cases Over Time", width = 6, plotOutput("trendPlot")),
      box(title = "State Comparison", width = 6, plotOutput("barPlot"))
    )
  )
)

# 🔹 SERVER
server <- function(input, output) {
  
  # 🔹 FILTER DATA
  filtered_data <- reactive({
    data %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2])
  })
  
  # 🔹 KPI CARDS
  output$total_cases <- renderValueBox({
    valueBox(
      format(sum(filtered_data()$confirmed, na.rm = TRUE), big.mark = ","),
      "Total Cases",
      icon = icon("virus"),
      color = "red"
    )
  })
  
  output$total_deaths <- renderValueBox({
    valueBox(
      format(sum(filtered_data()$deaths, na.rm = TRUE), big.mark = ","),
      "Total Deaths",
      icon = icon("skull"),
      color = "black"
    )
  })
  
  output$total_cured <- renderValueBox({
    valueBox(
      format(sum(filtered_data()$cured, na.rm = TRUE), big.mark = ","),
      "Total Recovered",
      icon = icon("heartbeat"),
      color = "green"
    )
  })
  
  # 🔹 TREND GRAPH (STATE-WISE)
  output$trendPlot <- renderPlot({
    filtered_data() %>%
      filter(state == input$state) %>%
      group_by(date) %>%
      summarise(total_cases = sum(confirmed, na.rm = TRUE)) %>%
      ggplot(aes(x = date, y = total_cases)) +
      geom_line(color = "blue", size = 1) +
      labs(title = paste("Cases Over Time -", input$state),
           x = "Date",
           y = "Confirmed Cases") +
      theme_minimal()
  })
  
  # 🔹 STATE COMPARISON GRAPH
  output$barPlot <- renderPlot({
    data %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2]) %>%
      group_by(state) %>%
      summarise(total_cases = max(confirmed)) %>%
      top_n(10, total_cases) %>%
      ggplot(aes(x = reorder(state, total_cases), y = total_cases)) +
      geom_bar(stat = "identity", fill = "orange") +
      coord_flip() +
      labs(title = "Top 10 States by Confirmed Cases",
           x = "State",
           y = "Cases") +
      theme_minimal()
  })
}

# 🔹 RUN APP
shinyApp(ui, server)