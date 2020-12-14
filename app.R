#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(jsonlite)
library(rvest)
library(data.table)
library(DT)
library(magrittr)
library(digest)
library(RPostgreSQL)
library(shinycssloaders)
library(shinythemes)
library(lubridate)
library(sf)
library(rgdal)
library(dplyr)
library(maps)
library(tidytext)
library(config)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(stringr)
library(zoo)
library(knitr)
library(tmap)
library(Rwordseg)
library(ggplot2)
library(maps)
library(wordcloud)
library(leaflet)
library(tidyverse)


gitR <- jsonlite::fromJSON("https://jobs.github.com/positions.json?description=R", flatten=TRUE)
gitR2 <- jsonlite::fromJSON("https://jobs.github.com/positions.json?description=R&page=2", flatten=TRUE)
gitR3 <- jsonlite::fromJSON("https://jobs.github.com/positions.json?description=R&page=3", flatten=TRUE)
gitJa <- jsonlite::fromJSON("https://jobs.github.com/positions.json?description=Java", flatten=TRUE)
gitJa2 <- jsonlite::fromJSON("https://jobs.github.com/positions.json?description=Java&page=2", flatten=TRUE)
gitPy <- jsonlite::fromJSON("https://jobs.github.com/positions.json?description=Python", flatten=TRUE)
gitSq<- jsonlite::fromJSON("https://jobs.github.com/positions.json?description=SQL", flatten=TRUE)
gitda <- jsonlite::fromJSON("https://jobs.github.com/positions.json?description=data", flatten=TRUE)
gitda2<- jsonlite::fromJSON("https://jobs.github.com/positions.json?description=data&page=2", flatten=TRUE)
job1<-rbind(gitR,gitR2,gitR3,gitPy,gitSq,gitJa,gitJa2,gitda,gitda2)
job1<-job1%>%separate_rows(location,sep=",")
job1<-job1%>%separate_rows(location,sep="/")
job1$location<-as.character(job1$location)
job1$location<-gsub(" ","",job1$location)

data("World")
World%<>%select(name,geometry)
World%<>%rename(country.etc=name)

data("world.cities")
world.cities$name<-gsub("'","",world.cities$name)
world.cities<-world.cities%>%select(name,country.etc)
world.cities%<>%rename(location=name)
job1<-left_join(job1,world.cities,by="location")


ids <- which(is.na(job1$country.etc))
job1$country.etc[ids] <- job1$location[ids]
job1$count<-1

job1$country.etc[which(job1$country.etc=="USA")]<-"United States"
job1$country.etc[which(job1$country.etc=="UK")]<-"United Kingdom"
job1$country.etc[which(job1$country.etc=="UnitedStates")]<-"United States"
job1$country.etc[which(job1$country.etc=="SanFrancisco|Remote(US/Canada)")]<-"United States,Canada"


job2<- job1%>% 
    group_by(country.etc) %>% 
    summarize(count = sum(count))

World<-left_join(World,job2,by="country.etc")
World[is.na(World)]<-0

tidy_word<- job1 %>%
    unnest_tokens(word, description)
data(stop_words)
tidy_word <- tidy_word %>%
    anti_join(stop_words)

tidy_word$word<-gsub("li","",tidy_word$word)
tidy_word$word<-gsub("ul","",tidy_word$word)
tidy_word$word<-gsub("und","",tidy_word$word)
tidy_word$word<-gsub("h2","",tidy_word$word)
tidy_count<-tidy_word %>%
    count(word, sort = TRUE)

button_color_css <- "

#DivCompClear, #FinderClear, #EnterTimes{

/* Change the background color of the update button

to blue. */

background: DodgerBlue;



/* Change the text size to 15 pixels. */

font-size: 15px;

}"

# Define UI

ui <- fluidPage(
    
    titlePanel("MA615 Final Project"),
    
    hr(),
    
    tags$h5("Created by Congyao Duan"),
    
    hr(),
    
    navbarPage("Statistical work distribution map& Discreption text mining",theme=shinytheme("lumen"),
               
               tabPanel("Interacitve Map",fluid=TRUE,icon=icon("world"),
                        
                        tmapOutput("map", width="100%", height="1000"),
                        
                        absolutePanel(id = "Select Panel", class = "panel panel-default", fixed = TRUE,
                                      
                                      draggable = TRUE, top = 200, left = "auto", right = 20, bottom = "auto",
                                      
                                      width = 330, height = "auto",
                                      
                                      
                                      
                                      h3("Jobs"),
                                      
                                      selectInput(inputId="WorldFinder", label="Countries",choices=unique(World$country.etc)),
                                      
                                      
                        ),
                        
                        
                        
               ),
               
               tabPanel("Data",icon=icon("table"),fluid=TRUE,
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                titlePanel("Job Opportunity Explorer"),
                                
                                fluidRow(
                                    
                                    selectInput(inputId="AnaLocationFinder",
                                                
                                                label="Select Location(s):",
                                                
                                                choices=unique(job1$country.etc))
                                    
                                )
                                
                            ),
                            
                            mainPanel(
                                
                                fluidRow(
                                    
                                    withSpinner(dataTableOutput(outputId="Table"))
                                    
                                ),
                                
                                
                                
                            ))
               ),
               
               
             #text mining
             tabPanel("Job discreption",
                      
                      titlePanel("Job discreption"),
                      
                      sidebarLayout(
                          
                          sidebarPanel( "The R language was used for text analysis 
                                        to draw a histogram of the most frequent words and word cloud"
                              
                             
                              
                              
                          ),
                          
                          mainPanel(
                              
                        plotOutput("Hist"),
                        plotOutput("wordcloud")
                              
                              
                              
                          ))
             ),
             
           
               ##More Info
               
               tabPanel("More",icon=icon("info-circle"),
                        
                        fluidRow(
                            
                            hr(),
                            
                            h3("About this Project"),
                            
                            h4("This project is the final project for class 615(Data Science in R) of MSSP program in Boston University."),
                            
                            h4("The project is intended to analysis the statistical jobs distrubution&job description."),
                            
                            h4(p("These data were collected from ",a("GITHUB Jobs",href="https://jobs.github.com/"),".")),
                            
                            hr(),
                            
                            h5("Built with",
                               
                               img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                               
                               "by",
                               
                               img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                               
                               ".")))
               
    )
    
)


server <- function(input, output,session) {
    
    
    output$map <- renderTmap({
        
        tmap_mode('view')
        tm_shape(World)+tm_polygons("count")
        
    }) 
    
    
    
    # labels <- sprintf(
    
    #     "%s ",
    
    #     Finder$disasterNumber
    
    # ) 
    
    observe({  
       
        
        
   Finder<-job1     
        
        
        
        
    })
    
    
    
    #Analysis
    
    #Analysis
    
    JobFinder<-reactive({
        
        req(input$AnaLocationFinder)
        
        filter(job1,country.etc %in% input$AnaLocationFinder)
        
    })
    
    
    
    
    
    output$Table<-renderDataTable({
        
        datatable(JobFinder())
        
    })
    
    
 # text mining
    
    
  output$Hist<-renderPlot({
      
      tidy_word %>%
          count(word, sort = TRUE) %>%
          filter(n >1000) %>%
          mutate(word = reorder(word, n)) %>%
          ggplot(aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip()
  })
  
  output$wordcloud<-renderPlot({
      
      wordcloud(tidy_count$word,tidy_count$n,random.order=FALSE,scale=c(5,1.5),colors = rainbow(80))
  })
    
    
    
}



# Run the application 

shinyApp(ui = ui, server = server)



