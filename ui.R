library(shiny)
library(plotly)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    
    .custom-box {
      color: white;
      padding: 20px;
      border-radius: 10px;
      text-align: center;
      font-size: 20px;
      font-weight: bold;
    }

    .cases-box {
      background: #e74c3c;
    }

    .deaths-box {
      background: #2c3e50;
    }

    .active-box {
      background: #27ae60;
    }

  "))
  ),
  titlePanel("Disease Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("disease", "Select Disease",
                  choices = unique(cleaned_data$Disease),
                  multiple = TRUE),
      
      selectInput("state", "Select State",
                  choices = unique(cleaned_data$State),
                  multiple = TRUE),
      
      sliderInput("yearRange", "Select Year Range",
                  min = min(cleaned_data$Year),
                  max = max(cleaned_data$Year),
                  value = c(min(cleaned_data$Year), max(cleaned_data$Year)))
    ),
    
    mainPanel(
      fluidRow(
        column(4, uiOutput("casesBox")),
        column(4, uiOutput("deathsBox")),
        column(4, uiOutput("activeBox"))
      ),
      tabsetPanel(
        
        tabPanel("Overview",
                 
                 h3("Summary"),
                 
                 fluidRow(
                   column(6, h4("Total Cases"), textOutput("totalCases")),
                   column(6, h4("Total Deaths"), textOutput("totalDeaths"))
                 ),
                 
                 br(),
                 h4("State-wise Summary"),
                 tableOutput("stateSummaryTable"),
                 
                 br(),
                 h4("Disease-wise Summary"),
                 tableOutput("diseaseSummaryTable")
        ),
        
        tabPanel("Cases",
                 plotlyOutput("casesPlot", height = "500px")
        ),
        
        tabPanel("Deaths",
                 plotlyOutput("deathsPlot", height = "500px")
        ),
        
        tabPanel("State Analysis",
                 plotlyOutput("stateMap"),
                 plotlyOutput("comparePlot")
        ),
        
        tabPanel("Prediction",
                 tableOutput("predTable")
        )
      )
    )
  )
)