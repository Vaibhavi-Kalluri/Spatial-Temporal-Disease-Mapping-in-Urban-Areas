library(shiny)

ui <- fluidPage(
  
  titlePanel("Disease Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("disease", "Select Disease",
                  choices = unique(cleaned_data$Disease)),
      
      selectInput("state", "Select State",
                  choices = unique(cleaned_data$State),
                  selected = unique(cleaned_data$State)[1]),
      
      sliderInput("yearRange", "Select Year Range",
                  min = min(cleaned_data$Year),
                  max = max(cleaned_data$Year),
                  value = c(min(cleaned_data$Year), max(cleaned_data$Year)))
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("Overview",
                 h3("Summary"),
                 fluidRow(
                   column(6, h4("Total Cases"), textOutput("totalCases")),
                   column(6, h4("Total Deaths"), textOutput("totalDeaths"))
                 )
        ),
        
        tabPanel("Cases",
                 br(),
                 plotOutput("casesPlot", height = "500px")
        ),
        
        tabPanel("Deaths",
                 br(),
                 plotOutput("deathsPlot", height = "500px")
        ),
        
        tabPanel("Prediction",
                 br(),
                 tableOutput("predTable")
        )
        
      )
    )
  )
)