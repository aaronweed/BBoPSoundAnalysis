# small app to view 2022 vocalizations at MABI by species at each site


library(tidyverse)
library(lubridate)
library(readxl)
library(shiny)


ui <-   fluidPage(    
  
  # Give the page a title
  titlePanel("Vocalizations by Species per plot at MABI"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      uiOutput("birdcodes"),
      hr(),
      helpText("Data from 2022 MABI Annotations"), width = 2),
    
    # Create a spot for the plot
    mainPanel(width = 12, 
              plotOutput("BirdPlot")  
    )
    
  )
)


server <- function(input, output) {
  
  MABI_SamplexSp <- read_excel("Data/MABI.n477.Sample-by-species.xlsx") 
  
  # Create set of reactive selection boxes in UI
  output$birdcodes <- renderUI({ 
    
    df_sub<-as.data.frame(colnames(select(MABI_SamplexSp, -plot,-date, -fileID)))
    
    selectInput(inputId='species', label='Species',  choices= df_sub, selected = "OVEN")
  })
  

  output$BirdPlot <- renderPlot({
    
    calls <-
      MABI_SamplexSp %>% dplyr::select(plot, date, aou2 = input$species) %>% 
        mutate(Date = lubridate::ymd(date))
    
    ggplot(data= calls, aes(x= Date, y = aou2, group = plot)) +
      labs(title = input$species, y = "Count")+
      geom_line()+
      facet_wrap(~plot)+
      theme_bw()+
      theme(axis.text = element_text(face="bold",size=14))+
      theme(axis.title = element_text(face="bold",size=16))+
      theme(strip.text.x = element_text(size = 14))
    
    
  }, height = 1000)
}


shinyApp(ui = ui, server = server)