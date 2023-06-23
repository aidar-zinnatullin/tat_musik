
# https://erlar.ru/asongs 

library(RCurl)
library(httr)
library(xml2)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(tidyverse) 
library(rvest)
library(stringi)
ccccccc <- read_html('https://erlar.ru/asongs')
name_song <- html_nodes(ccccccc, '.views-field-title a') %>% html_text() %>% unlist()
name_song

music <- html_nodes(ccccccc, 'td.views-field-tid') %>% html_text() %>% unlist()
music

lyrics <- html_nodes(ccccccc, 'td.views-field-tid-1') %>% html_text() %>% unlist()
lyrics

link_to <- html_nodes(ccccccc,'.views-field-title a')%>% 
  html_attr("href") %>%unlist()
link_to
df_first_page <- data.frame(
  name = name_song,
  composer = music,
  lyrics = lyrics,
  link_to = link_to,
  stringsAsFactors=F)


scrape_meta_info <- function(html){
  songs_info <- read_html(html)
  # variables that we're interested in
  name_song <- html_nodes(songs_info, '.views-field-title a') %>% html_text() %>% unlist()
  
  
  music <- html_nodes(songs_info, 'td.views-field-tid') %>% html_text() %>% unlist()
  
  lyrics <- html_nodes(songs_info, 'td.views-field-tid-1') %>% html_text() %>% unlist()
  
  
  link_to <- html_nodes(songs_info,'.views-field-title a')%>% 
    html_attr("href") %>%unlist()
  
  # putting together into a data frame
  df <- data.frame(
    name = name_song,
    composer = music,
    lyrics = lyrics,
    link_to = link_to,
    stringsAsFactors=F)
  return(df)
}

songs_website <- list()

base_url <- "https://erlar.ru/asongs?page="
pages <- c(1:121)
for (i in 1:length(pages)){
  # informative message about progress of loop
  message(i, '/', length(pages))
  # prepare URL
  url <- paste(base_url, pages[i], sep="")
  # scrape website
  songs_website[[i]] <- scrape_meta_info(url)
  # wait a couple of seconds between URL calls
  Sys.sleep(2)
}


songs_website <- do.call(rbind, songs_website)
songs_website <- rbind(df_first_page, songs_website)

# I need to create a real link adding the main website address
songs_website$link_to <-paste("https://erlar.ru", songs_website$link_to, sep = "")

save(songs_website, file = 'data/List_of_Songs.RData')


###### Now I need to go through all the links from songs_website
library(magrittr)
library(purrr)
testing_checkings <- read_html(songs_website$link_to[9])
html_nodes(testing_checkings, '#main p') %>% html_text() %>% unlist()
html_nodes(testing_checkings, '.last span') %>% html_text() %>% unlist()

tryCatch({html_nodes(testing_checkings,'.submitted')%>% html_text()}, error = function(e) {NA})
circo <- html_nodes(testing_checkings,'.datielettoriali > div , .datielettoriali h4') %>% html_text() %>% unlist()
circo[1]

scrape_more_info <- function(html){
  links <- read_html(html)
  # variables that we're interested in
  lyrics_text <- html_nodes(links, '#main p') %>% html_text() %>% unlist()
  
  title_song <- html_nodes(links,'.title') %>% html_text() %>% unlist()
  
  category <- html_nodes(links,'.category .category') %>% html_text() %>% unlist()
  
  rating <- html_nodes(links,'.fivestar-feedback-enabled') %>% html_text() %>% unlist()
  
  views <- html_nodes(links,'.last span') %>% html_text() %>% unlist()
  
    # putting together into a data frame
  df_more <- data.frame(
    lyrics_text = lyrics_text,
    name = title_song,
    category = category,
    rating = rating,
    views = views,
    stringsAsFactors=F)
  return(df_more)
}
more_info <- list()

for (i in 1:nrow(songs_website)){
  # informative message about progress of loop
  message(i, '/', nrow(songs_website))
  # prepare URL
  url <- songs_website[i,4]
  # scrape website
  more_info[[i]] <- scrape_more_info(url)
  # wait a couple of seconds between URL calls
  Sys.sleep(1)
} # 18:00 - 20:32
more_info_1 <- do.call(rbind, more_info)
n_distinct(more_info_1$name)
more_info_1 <- more_info_1 %>% 
  group_by(name) %>% 
  mutate(lyrics_text = paste0(lyrics_text, collapse = " ")) %>% 
  slice_head(n=1)
# need to merge separated cases
more_info_1$lyrics_text[1]

table(more_info_1$category)
save(more_info_1, file = 'data/Songs_Text.RData')



