


library(tidyverse)
library(lubridate)
library(readxl)
library(shiny)
library(magrittr)
library(DT)

server <- function(input, output) {
  
  #MABI_SamplexSp <- read_excel("Data/MABI.n477.Sample-by-species.xlsx") 
  
  output$BirdPlot <- renderPlot({
    
      long %>% dplyr::filter(Park %in% input$park & AOU %in% input$species)  %>% 
    
      ggplot(aes(x= month_day, y = Calls, group = plot, color= as.factor(year))) +
      labs(title = input$species, y = "Calls", x = "Day - Month", color = "Year")+
      geom_line(linewidth = 1)+
      facet_wrap(~plot)+
      theme_bw()+
      scale_color_discrete(type = c("black","blue"))+
      #scale_x_date(date_labels = "%b %d")+ # Displays month and day
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
      #theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
      theme(axis.text = element_text(size=12))+
      theme(axis.title = element_text(face="bold",size=16))+
      theme(strip.text.x = element_text(size = 14))+
      theme(legend.position = "top")+
      theme(legend.text = element_text(size= 16))
    
    
  }, height = 1000)
  
  # Render the data table
  output$dataTable <- renderDT({
    # Filter data based on selected park and species
    
    long %>% dplyr::filter(Park %in% input$park & AOU %in% input$species) %>% datatable(options = list(
      pageLength = 25))
    
   
  
  })
}