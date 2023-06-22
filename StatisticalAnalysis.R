
### Насколько различаются текст разных категорий с точки зрения смысла? Подсчитываем число уникальных слов к общему числу слов для каждой песни
### и вычисляем среднее значение для каждой категории.
library(tidyverse)
library(tidytext)
library(here)
library(stringi)
stemmed <-  read_csv("for_to_stemm.csv")
load(here("data", "List_of_Songs.RData"))
test_attempt <- left_join(stemmed, songs_website, by = "name")


count_words <- test_attempt %>%
  unnest_tokens(word, preproc) %>% 
  add_count(name, name = "total_words") %>%
  group_by(name, total_words) %>% 
  count(word, sort = TRUE) 
test_attempt$number_total_words <-  stri_count_words(test_attempt$preproc)
test_attempt$unique_words <- lengths(lapply(strsplit(test_attempt$preproc, split = ' '), unique))
test_attempt$fraction <- test_attempt$unique_words/test_attempt$number_total_words

genres_unique_words <-  test_attempt %>% group_by(category) %>% mutate(value_of_interest= mean(fraction)) %>% 
  slice_head(n = 1) %>% arrange(desc(value_of_interest)) %>% select(category, value_of_interest)


#### 
n_distinct(test_attempt$lyrics) #1200
n_distinct(test_attempt$composer) #870

### Какая музыка нравится больше? Надо вытащить количество голосов


