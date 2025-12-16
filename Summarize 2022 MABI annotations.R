
library(tidyverse)
library(lubridate)
library(readxl)


### Import Matt Ayres progress summary of sound fikel annotations at MABI

MABI_Sp <- read_excel("Data/MABI_speciesList.v03.xlsx", 
                                   sheet = "birds")

MABI_ann <- read_excel("Data/MABI.annotations.n88020.xlsx")

MABI_SamplexSp <- read_excel("Data/MABI.n477.Sample-by-species.xlsx")


ggplot(MABI_Sp, aes(x= nPlots, fill = code4))+
  geom_histogram(stat = "bin")


ggplot(MABI_Sp, aes(x= nVocals, fill = code4))+
  geom_histogram(stat = "bin")


# graph the no of vocalizations over the season
aou <- "REVI"
MABI_SamplexSp %>% select(plot, date, aou2= aou) %>% 
  mutate(Date = ymd(date)) %>% 
ggplot(aes(x= Date, y = aou2, group = plot)) +
  labs(title = aou, ylab= "Count")+
  geom_line()+
  facet_wrap(~plot)+
  theme_bw()

### Import NETN's point count data
pak::pkg_install("nationalparkservice/NPSutils")

library(NPSutils)

##Import point count data from a Data Package on DataStore

NPSutils::get_data_package(2305911) # downloads package locally - number is the datastore record code
dat <- NPSutils::load_data_package(2305911)   #This loads data from downloaded package CSV to a list
names(dat) <- gsub("pkg_2305911.", "", names(dat)) #removes first part of column name
VIEWS_NETN <- new.env() #creates a new environment
list2env(dat, envir = VIEWS_NETN) #moves edited list to the environment
rm(dat) #removes dat list since it is no longer needed

NETN.PC<-VIEWS_NETN$MIDN_NCBN_NETN_Landbirds_20240909 %>%  filter(GroupCode %in% "NETN")

NETN.PC %>% filter(UnitCode %in% "ACAD") %>% unique(SpeciesCode)

library(shiny)
#### small app below (also move to ui.R, server.R, and global.R)

ui <-   fluidPage(    
    
    # Give the page a title
    titlePanel("Vocalizations by Species"),
    
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
  

  # Fill in the spot we created for a plot
  output$BirdPlot <- renderPlot({
    
      MABI_SamplexSp %>% dplyr::select(plot, date, aou2 = input$species) %>% 
      mutate(Date = lubridate::ymd(date)) %>% 
      ggplot(aes(x= Date, y = aou2, group = plot)) +
      labs(title = input$species, ylab = "Count")+
      geom_line()+
      facet_wrap(~plot)+
      theme_bw()


  })
}


shinyApp(ui = ui, server = server)

