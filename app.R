# Winter Quarter 2022
# Final Project - GABRIEL ANGARITA
# Code 3: Shiny

# Library
library(sf)
library(tidyverse)
library(shiny)
library(plotly)
library(scales)
library(shinythemes)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(recipes)
library(ggplot2)


# Shiny app: https://gabrielangarita.shinyapps.io/final-project-gangaritateam/
  
# Defien options
crime_options_l = c("Homicidies", "Robbery", "Domestic_Violence")
education_options_l = c("Dropout_Men", "Dropout_Women", "Number_Schools")

crime_options = c("Homicidies", "Robbery", "Domestic Violence")
education_options = c("Girls Dropout Rate", "Boys Dropout Rate", "Number of Schools")

ui = fluidPage(
  navbarPage("Crime and Education in Bogotá",
             tabPanel("Introduction",
                      
                      fluidPage(theme=shinytheme("flatly"),
                                h1("Welcome!"),
                                p("This DashsBoard presents information to analyze the relationship between 
                                  crime and educational outcomes at the level of the Urban Planning Unit for Bogotá. 
                                  The objective is to identify if places with high crime present low educational performance."),
                                br(),
                                p(strong(tags$u("Bogotá"))),
                                p("Capital City of Colombia"),  
                                br(),
                                p(strong(tags$u("Urban Planning Units - UPZ"))),
                                p("It is a geographical unit that groups neighborhoods. This unit serves as a tool for urban planning"),
                                br(),
                                p(strong(tags$u("Crime Data"))),
                                p("Statistics on homocidies, robbery and domestic violence. Data for year 2019. 
                                   Source: Bogotá Council"),  
                                br(),
                                p(strong(tags$u("Education Data"))),
                                p("Statistics on dropout rate of school.Data for year 2019. Source: Bogotá Council"),  
                                br()
                      )),
             
             tabPanel("Maps Crime",
                      tabname="Maps Crime",
                      
                      fluidPage(
                        fluidRow(
                          column(width = 12,
                                 selectInput(inputId = "crime_v", label = "Crime Variables", 
                                 choices = crime_options)
                                 )
                          ),
                        
                        fluidRow(width = 10,
                                 plotlyOutput("ts_crime")
                                 )
                      )
                      
             ),
             
             tabPanel("Maps Education",
                      tabname="Maps Education",
                      
                      fluidPage(
                        fluidRow(
                          column(width = 12,
                                 selectInput(inputId = "educ_v", label = "Education Variables", 
                                 choices = education_options)
                                 )
                          ),
                          fluidRow(width = 10,
                                   plotlyOutput("ts_education")
                          )
                        )
                      
             ),
             
             tabPanel("Regression Analysis",
                      tabname="regression",
                      
                      fluidPage(
                        fluidRow(
                          uiOutput("value"),
                          uiOutput("value2")
                        ),
                        
                        fluidRow(width = 10,
                                 plotlyOutput("resume"),
                         )
                        )
                      
             )
             
  ))

server <- function(input, output, session) {
  
  # Crime---------------
  
  # Load data Dropout rate
    crime <- st_read("Data/Clean/Crime/crime.shp")
  
    data_crime <- reactive({
    filter(crime, Indicator == input$crime_v)
  })
  
  output$ts_crime <- renderPlotly({
    
    crime_map <- 
      
      ggplot() + geom_sf(data_crime() , mapping = aes(fill = Total)) +  
      labs(title = "Crime by Urban Planning Units ", 
           subtitle = "",
           fill = element_blank(),
           caption = "Source: Bogotá City") + 
      theme_void() +  coord_sf() + theme(axis.title = element_text(size=8)) +
      scale_fill_distiller(name="Total Cases", palette = "Spectral", breaks = pretty_breaks())
    
    ggplotly(crime_map)
    
  })
  
  # Education---------------
  
  # Load data Dropout rate
  path <- ("Data/Clean/Education")
  edu <- st_read(file.path(path, "edu.shp"))
  
  data_edu <- reactive({
    filter(edu, Indicator == input$educ_v)
  })
  
  output$ts_education <- renderPlotly({
      
      edu_map <- 
        
        ggplot() + geom_sf(data_edu(), mapping = aes(fill = Total)) +  
        labs(title = "Education by Urban Planning Units", 
             subtitle = "",
             fill = element_blank(),
             caption = "Source: Bogotá City") + 
        theme_void() +  coord_sf() + theme(axis.title = element_text(size=8)) +
        scale_fill_distiller(name="", palette = "Spectral", breaks = pretty_breaks())
      
      ggplotly(edu_map)
      
    })
    
  # Regresion--------------
  
  # Load data
  df_reg <- read.csv("Data/Clean/Analysis/regression_analysis.csv")
  
  output$value<-renderUI({

    selectInput(
      
      inputId = "y", 
      label = "Crime Variables", 
      choices = crime_options_l
      
    )
    
  })
  
  output$value2<-renderUI({
    
    selectInput(
      inputId = "x", 
      label = "Education Variables", 
      choices = education_options_l
      
    )
  })
  
  data <- reactive({
    
    data_y <- select(df_reg, input$y)

    data_x <- select(df_reg, input$x)
    
    data <- cbind(data_y,data_x)
   
  })
  
  output$resume <- renderPlotly({
    
   p <-  ggplot(NULL, aes(x = as.double(data()[,2]), y = as.double(data()[,1]))) +
         geom_point() +
         stat_smooth(method = "lm") +
         labs(x = input$x, y = input$y) + theme_bw()
    
   ggplotly(p)
   
  })
  
  
}

shinyApp(ui = ui, server = server)
