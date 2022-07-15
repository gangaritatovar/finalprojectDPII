# Winter Quarter 2022
# Final Project - GABRIEL ANGARITA
# Code 2: Text Analysis

# loading the packages:
library(syuzhet)
library(rvest)
library(dplyr) 
library (xml2)
library(dplyr) 
library(rvest) 
library(stringr)
library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(lubridate)

# Setting up the working directory
setwd("~/Documents/GitHub/final-project-gangaritateam")

#---------------------------------
# 1. Extracting news from Bogota
#---------------------------------
# Extract news from Google - Search "Bogota Crime"
news_en <- read_html("https://news.google.com/search?q=Bogota&hl=en-US&gl=US&ceid=US%3Aen")

# Get headlines
headlines <- data.frame(title = news_en %>% 
                          html_nodes('.DY5T1d') %>% 
                          html_text())

# Get time of each new
prod <- html_nodes(news_en, ".SVJrMe")

time <- lapply(prod, function(x) {
  norm <- tryCatch(html_node(x, "time") %>% html_text(),
                   error=function(err) {NA})
})

# Create a unique data frame
time <- data.frame(time = do.call(rbind, time), stringsAsFactors = F)
headlines <- cbind(headlines, time)

# Help: 
# https://www.listendata.com/2020/12/web-scrape-google-news-with-r.html
# https://allanvc.github.io/post/2018-08-21-google_news_scraping/

#---------------------------------
# 2. Analysis Sentiment:
#---------------------------------

# Get sentiment
sentiment_nrc <-   get_sentiments("nrc")   #manually created via crowdsourcing, ten categories, not unique!
sentiment_afinn <- get_sentiments("afinn") #product of one researcher, pos/neg integer values
sentiment_bing <-  get_sentiments("bing")  #built on online reviews, pos/neg only

# Get text from headlines
text_df <- as.character(headlines$title)
text_df <- tibble(text = text_df)

# Set words
word_tokens_df <-     unnest_tokens(text_df, word_tokens,  text, token = "words")

# Drop stop words
no_sw <- anti_join(word_tokens_df, stop_words, by = c("word_tokens" = "word"))

# Joint sentiments
for (s in c("nrc" , "bing", "afinn")) {
  
  no_sw <- no_sw %>%
    left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
    plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
  
}

# Pivot - longer data
no_sw$afinn <- as.character(no_sw$afinn)
no_sw_l <- pivot_longer(no_sw, cols = "nrc":"afinn" , names_to = "indicator", values_to = "Total")

#  Labels categories
no_sw_l <- no_sw_l %>%
           mutate(Type = case_when(indicator=="nrc" ~ "NRC Emotion Lexicon",
                                   indicator=="afinn" ~ "Afinn Lexicon",
                                   indicator=="bing" ~ "Bing Lexicon"))
# Plot
text_analysis_plot <- ggplot(data = filter(no_sw_l, !is.na(Total))) +
                      geom_histogram(aes(Total), stat = "count", na.rm = TRUE) +
                      scale_x_discrete(guide = guide_axis(angle = 90)) + 
                      labs(title = "Sentiment Analysis: News from Bogotá", subtitle = "Three general-purpose lexicons", 
                        caption = "Afinn lexicon assigns words with a score that runs between -5 and 5, with negative scores indicating negative sentiment and positive scores indicating positive sentiment.") +
                        ylab("Number of words") +
                        xlab("Sentiment Analysis Scale") +
                      facet_grid(~Type, scales = "free") + scale_colour_discrete(drop = FALSE) +
                      theme_light()

# Save plot 
ggsave("Images/Text Analysis.png", width = 10, height = 4)
