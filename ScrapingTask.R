
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

save(songs_website, file = 'List_of_Songs.RData')


###### Now I need to go through all the links from deputies_website
library(magrittr)
testing_checkings <- read_html(deputies_website$link_to[2])


name
circo <- html_nodes(testing_checkings,'.datielettoriali > div , .datielettoriali h4') %>% html_text() %>% unlist()
circo[1]

scrape_more_info <- function(html){
  links <- read_html(html)
  # variables that we're interested in
  name <- html_nodes(links, 'h3') %>% html_text() %>% unlist()
  
  circo <- html_nodes(links,'.datielettoriali > div , .datielettoriali h4') %>% html_text() %>% extract2(1)
  
  # putting together into a data frame
  df <- data.frame(
    name = name,
    circo = circo,
    stringsAsFactors=F)
  return(df)
}
more_info <- list()

for (i in 1:nrow(deputies_website)){
  # informative message about progress of loop
  message(i, '/', nrow(deputies_website))
  # prepare URL
  url <- deputies_website[i,3]
  # scrape website
  more_info[[i]] <- scrape_more_info(url)
  # wait a couple of seconds between URL calls
  Sys.sleep(0.5)
}
more_info_1 <- do.call(rbind, more_info)
more_info_2 <- more_info_1[seq(3, 1200, by = 3),]
#n_distinct(more_info_2$name)
#n_distinct(more_info_2$circo)

#duplicated_stuff <- more_info_2[duplicated(more_info_2$circo),]

save(more_info_2, file = 'Scraped/deputies_additional_info_camera_all.RData')





#####
#####
#####
####### Packages

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

ccccccc <- read_html('https://www.senato.it/leg/19/BGT/Schede/Attsen/Sena.html')


name <- html_nodes(ccccccc, 'p:nth-child(1) a') %>% html_text() %>% unlist()
name

party <- html_nodes(ccccccc, 'p+ p a') %>% html_text() %>% unlist()
party

link_to <- html_nodes(ccccccc,'p:nth-child(1) a')%>% 
  html_attr("href") %>%unlist()
link_to


scrape_senato <- function(html){
  senators <- read_html(html)
  # variables that we're interested in
  name <- html_nodes(senators, 'p:nth-child(1) a') %>% html_text() %>% unlist()
  
  party <- html_nodes(senators, 'p+ p a') %>% html_text() %>% unlist()
  
  link_to <- html_nodes(senators,'p:nth-child(1) a')%>% 
    html_attr("href") %>%unlist()
  
  # putting together into a data frame
  df <- data.frame(
    name = name,
    party = party,
    link_to = link_to,
    stringsAsFactors=F)
  return(df)
}

senators_website <- list()

base_url <- "https://www.senato.it/leg/19/BGT/Schede/Attsen/Sen"
pages <- c('a', 'b', 'c', 'd', 'f', 'g', 'i', 'l', 'm', 'n', 'o', 'p', 'r', 's', 't', 'u', 'v', 'z')
for (i in 1:length(pages)){
  # informative message about progress of loop
  message(i, '/', length(pages))
  # prepare URL
  url <- paste(base_url, paste(pages[i], '.html', sep = ""), sep="")
  # scrape website
  senators_website[[i]] <- scrape_senato(url)
  # wait a couple of seconds between URL calls
  Sys.sleep(2)
}

#url_testttsss <- paste(base_url, paste(pages[1], '.html', sep = ""), sep="")
#url_testttsss
senators_website <- do.call(rbind, senators_website)
n_distinct(senators_website$name)
n_distinct(senators_website$party)
n_distinct(senators_website$link_to)

senators_website$link_to_2 <- gsub(pattern = '/loc/link.asp?tipodoc=sattsen&leg=19&id=', replacement = '', x = senators_website$link_to, fixed = TRUE)

save(senators_website, file = 'Scraped/senato_website_camera_all.RData')




###### Now I need to go through all the links from deputies_website
library(magrittr)
library(stringr)
senators_website$link_to_3 <- str_pad(senators_website$link_to_2, 8, pad = "0", side = "left")
senators_website$link_to_4 <- paste(paste('https://www.senato.it/leg/19/BGT/Schede/Attsen/', senators_website$link_to_3, sep = ''), '.htm', sep = '')
testing_checkings <- read_html(senators_website$link_to_4[2])
#'https://www.senato.it/leg/19/BGT/Schede/Attsen/00000032.htm' # 8 digits

name <- html_nodes(testing_checkings, '.titolo') %>% html_text() %>% unlist()
name
circo <- html_nodes(testing_checkings,'p:nth-child(1)') %>% html_text() %>% extract2(2)
circo[2]

scrape_more_info_senate <- function(html){
  links <- read_html(html)
  # variables that we're interested in
  name <- html_nodes(links, '.titolo') %>% html_text() %>% unlist()
  
  circo <- html_nodes(links,'p:nth-child(1)') %>% html_text() %>% extract2(2)
  
  # putting together into a data frame
  df <- data.frame(
    name = name,
    circo = circo,
    stringsAsFactors=F)
  return(df)
}
more_info_senate <- list()

for (i in 1:nrow(senators_website)){
  # informative message about progress of loop
  message(i, '/', nrow(senators_website))
  # prepare URL
  url <- senators_website[i,6]
  # scrape website
  more_info_senate[[i]] <- scrape_more_info_senate(url)
  # wait a couple of seconds between URL calls
  Sys.sleep(0.5)
}
more_info_senate_1 <- do.call(rbind, more_info_senate)
#more_info_senate_2 <- more_info_senate_1[seq(3, 1200, by = 3),]
n_distinct(more_info_senate_1$name)
n_distinct(more_info_senate_1$circo)

#duplicated_stuff <- more_info_senate_2[duplicated(more_info_senate_2$circo),]

save(more_info_senate_1, file = 'Scraped/senators_additional_info_all.RData')

#####
##### Load everything and check
#####
load('Scraped/deputies_additional_info_camera_all.RData')
load('Scraped/deputies_website_camera_all.RData')
load('Scraped/senato_website_camera_all.RData')
load('Scraped/senators_additional_info_all.RData')

more_info_2$circo[2]
a_unnest <- more_info_2 %>% 
  mutate(circo = strsplit(circo, '\r\n\r\n')) %>%
  unnest(circo) %>% 
  filter(str_detect(pattern = 'della coalizione', circo, negate = TRUE))

#returing <- a_unnest %>% separate(circo, sep = c(), into = c('Region', 'Constituency', 'Present', 'Ploclamation'))
#library(magrittr)
#returing <- a_unnest %>% group_by(name) %>%  nest()


returning <- more_info_2 %>% separate(circo, sep = '\r\n\r\n', into = c('Region', 'Constituency', 'Present', 'Ploclamation'))
returning$Region <- gsub(pattern = 'Eletto nella circoscrizione ', replacement = '', x = returning$Region, fixed = TRUE)
returning$Region <- gsub(pattern = 'Eletta nella circoscrizione ', replacement = '', x = returning$Region, fixed = TRUE)

returning$Constituency[2]
returning$Constituency <- gsub(pattern = 'Collegio di elezione collegio uninominale ', replacement = '', x = returning$Constituency, fixed = TRUE)
returning$Constituency <- gsub(pattern = 'Collegio di elezione collegio plurinominale ', replacement = '', x = returning$Constituency, fixed = TRUE)
returning$Constituency <- gsub(pattern = 'Collegio di elezione plurinominale ', replacement = '', x = returning$Constituency, fixed = TRUE)

returning$Present <- gsub(pattern = 'Presentato dalla Lista  ', replacement = '', x = returning$Present, fixed = TRUE)
returning$Present <- gsub(pattern = 'Lista di elezione ', replacement = '', x = returning$Present, fixed = TRUE)
returning$Present <- gsub(pattern = 'Presentata dalla Lista  ', replacement = '', x = returning$Present, fixed = TRUE)
returning$Present <- gsub(pattern = 'Presentato dalle liste collegate in coalizione ', replacement = '', x = returning$Present, fixed = TRUE)
returning$Present <- gsub(pattern = 'Presentata dalle liste collegate in coalizione ', replacement = '', x = returning$Present, fixed = TRUE)

returning$Ploclamation <- NULL

deputies <- deputies_website %>% bind_cols(., returning)
save(deputies, file = 'Scraped/deputies.RData')

####
#### For Senato
####
more_info_senate_1$circo <- str_trim(more_info_senate_1$circo, side = 'both')
more_info_senate_1$circo[1]

more_info_senate_1$circo_2 <- gsub(".*Regione di elezione: \\s*|Nat.*", "", more_info_senate_1$circo)
senators <- more_info_senate_1 %>% bind_cols(., senators_website)
save(senators, file = 'Scraped/senators.RData')


