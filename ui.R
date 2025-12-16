

library(shiny)

library(DT)


ui <- fluidPage(
  
  # Give the page a title
  titlePanel("Vocalizations by Species per plot"),
  
  # Generate a row with a sidebar
  sidebarLayout(
    
    # Define the sidebar
    sidebarPanel(
      fluidRow(
        column(6, 
               selectInput(inputId='park', label='Select Park', 
                           choices= Park_list, selectize = TRUE, selected = "MABI")
        ),
        column(6, 
               selectInput(inputId='species', label='Select Species', 
                           choices= AOU_list, selectize = TRUE, selected = "BTNW")
        )
      ),
      hr(),
      helpText("Data from 2022 and 2023 Annotations"),
      width = 2
    ),
    
    # Create a spot for the plot and data table in tabs
    mainPanel(
      width = 10, 
      tabsetPanel(
        tabPanel("Plot", 
                 plotOutput("BirdPlot")
        ),
        tabPanel("Data Table", 
                 DT::DTOutput("dataTable")  # Output for the data table
        )
      )
    )
    
  )
)