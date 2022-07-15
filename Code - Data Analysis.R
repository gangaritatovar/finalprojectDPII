# Winter Quarter 2022
# Final Project - GABRIEL ANGARITA
# Code 1: Data Analysis

# Library
library(sf)
library(tidyverse)
library(spData)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(readxl)
library(aplot)
library(cowplot)

# Setting up the working directory
setwd("~/Documents/GitHub/final-project-gangaritateam")

#------------------------
# Set and clean data
#------------------------

#---------------
# 0. Population
#---------------

# Load data
population <- read_excel("Data/Raw/General/Population/population_UPZ.xlsx", sheet = "1.1", skip = 5)

# Filter and select UPZ code
population <- population %>% 
              rename("UPZ" = `Código UPZ`,"Population" = `2019`) %>%
              select(UPZ, Population) %>% 
              filter(UPZ !="") 

#-------------------------------------
# 1. Frame: Urban Planning Units - UPZ
#-------------------------------------

# Load data
path <- ("~/Documents/GitHub/final-project-gangaritateam/Data/Raw/General/UPZ")
UPZ <- st_read(file.path(path, "UPla.shp"))

# Organice UPZ variable
UPZ <- UPZ %>% 
       filter(UPlTipo !=2, ) %>%
       mutate(UPZ = str_replace_all(UPlCodigo,"UPZ",""),
              UPZ = as.numeric(UPZ)) 

# Include population
UPZ <- UPZ %>% left_join(population, by = c("UPZ"))

# Save clean data
st_write(UPZ, "Data/Clean/General/UPZ_frame.shp")

# Create map 0
map0 = ggplot() + geom_sf(data = UPZ , mapping = aes(fill = Population)) +  
       labs(title = "Population in Bogotá by Urban Planning Units (UPZ)", 
       subtitle = "",
       fill = element_blank(),
       caption = "Source: Bogotá City") + 
       theme_void() +  coord_sf() + theme(axis.title = element_text(size=8)) +
       scale_fill_distiller(name="Population (in levels)", palette = "Spectral", breaks = pretty_breaks())

# Save map 0
ggsave("Images/Map 0 Population.png", 
       map0,
       bg = "white", 
       width=9, height=12)

#---------
# 2. Crime
#---------

# Load data Crime
path <- ("~/Documents/GitHub/final-project-gangaritateam/Data/Raw/Crime/Crime Data")
crime <- st_read(file.path(path, "DAIUPZ.shp"))

#--------------------
# 2.a Police Stations
#--------------------

# Set data Police Stations
path <- ("~/Documents/GitHub/final-project-gangaritateam/Data/Raw/Crime/Police")
police <- st_read(file.path(path, "EstacionPolicia.shp"))

#----------------------------------------
# 2.b Crime Data: organice and clean data
#----------------------------------------

# Select: homicides, robbery, domestic violence, drugs - Rename
crime <- crime %>% select(CMIUUPLA,CMNOMUPLA, CMH19CONT, CMHP19CONT, 
                          CMVI19CONT, SHAPE_AREA, SHAPE_LEN, geometry) %>%
                          rename("UPZ" = "CMIUUPLA",
                                 "Name" = "CMNOMUPLA",
                                 "Homicidies" = "CMH19CONT",
                                 "Robbery" = "CMHP19CONT",
                                 "Domestic_Violence" = "CMVI19CONT")

# Organice UPZ variable
crime <- crime %>% mutate(UPZ = str_replace_all(UPZ,"UPZ",""),
                          UPZ = as.numeric(UPZ))

# Filter: no localization crimes and rural zone
crime <- crime %>% 
         filter(UPZ != 999, UPZ !="") %>%
         mutate(UPZ = as.factor(UPZ))

# Reshape
crime_1 <- as.data.frame(crime)
crime_1 <- pivot_longer(crime_1, cols = "Homicidies":"Domestic_Violence", 
                        names_to = "Indicator", values_to = "Total")

# Create Categories for map
crime_1 <- crime_1 %>% mutate(Indicator = case_when(Indicator=="Domestic_Violence" ~ "Domestic Violence",
                                                    Indicator=="Homicidies" ~ "Homicidies",
                                                    Indicator=="Robbery" ~ "Robbery"))

# Transform to a file st
crime_final <- st_sf(crime_1)

# Save data
st_write(crime_final, "Data/Clean/Crime/crime.shp", append = FALSE)

# Create crime df to merge with education data for analysis regression
crime_final_1 <- crime %>% 
                 as.data.frame() %>%
                 select(-geometry)

# Save clean data to regression analysis
write.csv(crime_final_1,"Data/Clean/Crime/crime_analysis.csv")

#----------------------------------
# 2.c Create maps by type of crime
#----------------------------------

# Names variables of each maps
variables <- c("Homicidies", "Robbery", "Domestic Violence")

# Create each map
maps_crime <- map(.x = variables, 
            .f = function(x) crime_final %>% 
              filter(Indicator == x) %>% 
              ggplot() + geom_sf(mapping = aes(fill = Total)) +
              geom_point(data = police, aes(x = EPOLONGITU, y = EPOLATITUD), color = "red") +  
              labs(fill = element_blank()) + 
              theme_void() + coord_sf() + 
              scale_fill_distiller(name="Total cases", palette = "Spectral", breaks = pretty_breaks()))

# Create grid
maps_crime <- plot_grid(plotlist = maps_crime, label_size = 10, nrow = 1, labels = c(variables))

# Set title
title_gg <- ggplot() + labs(title = "Crime by Urban Planning Units", 
                            subtitle = "Police Stations (red points)") 

# Final plot
map1 <- plot_grid(title_gg, maps_crime, ncol = 1,rel_heights = c(0.1, 1))

# Save map 1
ggsave("Images/Map 1 Crime.png", 
       map1,
       bg = "white",
       width=15, height=7)

#---------------
# 3. Education
#---------------

#-------------
# 3.a Schools
#-------------

# Load data Schools
path <- ("~/Documents/GitHub/final-project-gangaritateam/Data/Raw/Education/Colegios")
edu_1 <- st_read(file.path(path, "Colegios.shp"))

# Summarise total schools by UPZ
edu_1 <- edu_1 %>% 
               group_by(COD_UPZ) %>%
               summarise(Number_Schools = n()) %>%
               rename("UPZ" = "COD_UPZ") %>% 
               as.data.frame() %>%
               select(-geometry)

#-----------------
# 3.b Dropout rate
#-----------------

# Load data Dropout rate
path <- ("~/Documents/GitHub/final-project-gangaritateam/Data/Raw/Education/Desercion")
edu_2 <- st_read(file.path(path, "Tasa_Desercion_Oficial_UPZ.shp"))

# Select: school dropout rate
edu_2 <- edu_2 %>% filter(COD_UPZ!="1",COD_UPZ!="2", COD_UPZ!="3", COD_UPZ!="4", COD_UPZ!="5") %>%
                   select(COD_UPZ, Thombre_UP,Tmujer_UPZ) %>% 
                   rename("UPZ" = "COD_UPZ",
                         "Dropout_Women" = "Tmujer_UPZ",
                         "Dropout_Men" = "Thombre_UP")

# Organice UPZ variable
edu_2 <- edu_2 %>% mutate(UPZ = str_replace_all(UPZ,"UPZ",""),
                          UPZ = as.numeric(UPZ)) %>% 
                   filter(UPZ != 999,
                          UPZ !="") %>% select(-geometry) 

# Data frame
edu_2 <- as.data.frame(edu_2)

# Merge with Schools
edu_2 <- edu_2 %>% mutate(UPZ = as.factor(UPZ)) %>% 
                   left_join(edu_1, by = "UPZ") %>%
                   relocate("UPZ", "Dropout_Men", "Dropout_Women", "Number_Schools", "geometry") 

# Reshape
edu_final <- pivot_longer(edu_2, cols = "Dropout_Men":"Number_Schools", 
                          names_to = "Indicator", values_to = "Total")

# Create Categories for map
edu_final <- edu_final %>% mutate(Indicator = case_when(Indicator=="Dropout_Women" ~ "Girls Dropout Rate",
                                                        Indicator=="Dropout_Men" ~ "Boys Dropout Rate",
                                                        Indicator=="Number_Schools" ~ "Number of Schools"))

# Create st data
edu_final <- st_sf(edu_final)

# Save data st
st_write(edu_final, "Data/Clean/Education/edu.shp", append = FALSE)

# Save clean data to regression analysis
write.csv(edu_2,"Data/Clean/Education/education_analysis.csv")

#------------------------------------
# 3.c Score standardized Test Results
#------------------------------------

# Load data Test Results
path <- ("~/Documents/GitHub/final-project-gangaritateam/Data/Raw/Education/Saber")
edu_3 <- st_read(file.path(path, "ResultadoSaber11_2019.shp"))

# Select: test variable and organice data frame
edu_3 <- edu_3 %>% select(OBJECTID, P_PUNTAJE, SECTOR)   %>% 
                   mutate(lat = unlist(map(edu_3$geometry,1)),
                          long = unlist(map(edu_3$geometry,2))) %>%
                   filter(P_PUNTAJE > 0, long > 4.45)
  
# Reshape
edu_3 <- as.data.frame(edu_3)
edu_3 <- pivot_longer(edu_3, cols = "P_PUNTAJE", 
                      names_to = "Indicator", values_to = "Total")

# Transform to a file st
edu_3 <- st_sf(edu_3)

# Create Categories for map
edu_3 <- edu_3 %>% mutate(Type = case_when(SECTOR==2 ~ "Public Schools",
                                           SECTOR==1 ~ "Private Schools"))

# Map 2
map2 =  ggplot() + geom_sf(data= UPZ) + geom_sf(data=edu_3, aes(color=Total)) +
        facet_wrap(~Type) +
        labs(title = "Results in school performance by type of school", 
             subtitle = "Each point represents a school",
             fill = element_blank(),
             caption = "Source: Bogotá City") +
        coord_sf() +  theme_void() +
        scale_color_distiller(name="Score standardized Test Results", 
                              palette = "Paired", breaks = pretty_breaks())

# Save map 2
ggsave("Images/Map 2 Education.png",
       map2,
       bg = "white",
       width=12, height=9.5)

#-------------------------
# 4. Analysis - Regression 
#-------------------------

#----------------------------
# 4.a Crime and Education data
#-----------------------------

# Get data frame: all the variables
df <- crime_final_1 %>% 
      left_join(edu_2, by= "UPZ") %>% 
      select(UPZ, Dropout_Men, Dropout_Women, Number_Schools,
             Homicidies, Robbery, Domestic_Violence)

# Save clean data
write.csv(df,"Data/Clean/Analysis/regression_analysis.csv")


