Crime and Education in Bogotá
================

## Project Description

The objective of this project is to describe the relationship between
crime and education in Bogotá. This project uses the geographic data of
Bogotá at the level of the Urban Planning Unit (UPZ), which corresponds
to a geographic classification that brings together a set of
neighborhoods.

### Research question

The purpose of this project is to review the geographic distribution of
two relevant issues for public policy: education and crime. This is very
relevant to see the behavior of the variables in a more detailed way and
not only at an aggregate level. Secondly, investigate at a geographical
level the relationship between crime and education in Bogotá, a city of
more than 9 million inhabitants and the capital of Colombia.

This question is relevant to partially understand the determinants of
crime, associated, for example, with the low provision of public goods.
Or on the contrary, if the crime, such as theft, has a more strategic
behavior in terms of being located in more central areas of the city.
Likewise, a variable such as domestic violence is reviewed, to inquire
about another type of crime, with reasons other than homicide or
robbery.

## Data

The data of the project correspond to the repository of the city of
Bogotá with geographical information. The unit of analysis of the data
correspond to UPZ. In general, the city has a wide set of data, these
cover different topics. The data has an advantage due to its easy
access, it has a classification of the identifiers that is transversal
to the files. However, one of the difficulties is that the dictionaries
of the variables are not very clear and do not correctly detail the
names of the variables to the shp files. Link:
<https://datosabiertos.bogota.gov.co/>

### 1\. Raw Data

This folder contains the raw files:

#### General:

``` 
    a. Population
    b. UPZ file: shp file basic geographical framework
```

#### Crime:

``` 
    a. Police: geographical ubication of each police station
    b. Crime: crime statistics by UPZ
    c. Support: dictonary of the shp file
```

#### Education:

``` 
    a. Colegio: detail of each school in Boogtá
    b. Deserción: dropout rate of school by UPZ 
    c. Saber: score of standarized test for each school
    d. Support: dicctonary of the shp files
```

### 2\. Clean Data

This folder contains the clean files for:

#### General:

``` 
    a. UPZ shp file: basic geographical framework
```

#### Crime:

``` 
    a. Crime shp file: crime statistics by UPZ for the shiny app
    b. Dataframe for regression analysis
```

#### Education:

``` 
    a. Education shp file: dropout rate statistics by UPZ for the shiny app
    b. Dataframe for regression analysis
    
```

##### Analysis:

``` 
    a. Dataframe for regression analysis
```

## Code

### 1\. Data Analysis

This code contains the organization and cleaning of the data. First, the
general data, crime and education were cleaned. Later they were
organized to create the maps and save the clean data to be used in the
Shiny application.

### 2\. Text Analysis

The code performs the text analysis of the main news headlines
associated with Bogotá.In the first place, the headlines of the main 100
news from Bogotá in recent months are scrapped. Subsequently, the detail
on the feelings of the words for three dictionaries of feelings is
reviewed. In general, a marked negativity is observed in the “Affin” and
“Bing” lexicons, while in the NRC lexicon a large number of positive
words are observed. One of the difficulties in doing text analysis is
the limitation to find sentiment dictionaries in Spanish, the options
being very limited. This was a difficulty to analyze in more detail for
example the local press.

### 3\. Shiny app

The code has the Shiny application. The app contains three panels. The
first shows the maps by type of crime by UPZ. The second shows the maps
for educational variables. Finally, a plotly is presented with a linear
regression between the crime and educational variables. This last
analysis is performed at the UPZ level and shows the positive
correlation between the variables of crimen and education.

Link:
<https://gabrielangarita.shinyapps.io/final-project-gangaritateam/>

## Images

Folder with the plots:

1.  Map 0: map of population by UPZ
2.  Map 1: map of crimes by UPZ
3.  Map 2: map of score of standarized test for each school
4.  Text Analysis: summary of sentiments in the news from Bogotá

## Results

This project presents a geographical description of crime and education
in Bogotá. In general, crime is concentrated in certain areas of the
city, especially homicides and domestic violence in the most extreme
areas, while robberies are more concentrated in the central area. In the
case of education, a spatial dynamic is also observed in which poor
school performance is concentrated in the extreme areas of the city. Map
2 shows a very strong inequality in performance between public and
private schools, the latter more concentrated in the northern part of
the city. The results indicate a relationship between certain crimes,
such as homicides and domestic violence, with lower school performance.
The next step should be to review this information at a more detailed
geographic level, because the UPZ is also an aggregation of
neighborhoods, which may have different dynamics. Finally, the analysis
of sentimental text shows mixed results. In general presenting a very
negative idea of the city but in contrast to news with terms associated
with issues of trust.
