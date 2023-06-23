
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
n_distinct(test_attempt$lyrics) #1201
n_distinct(test_attempt$composer) #870

### Какая музыка нравится больше? Надо вытащить количество голосов
# I have to use regex to derive number of votes within brackets
test_attempt$votes <- str_extract_all(pattern ="\\([^()]+\\)", test_attempt$rating)
test_attempt$votes <- substring(test_attempt$votes, 2, nchar(test_attempt$votes)-1)


toMatch <- c("голос", "голоса", "голосов")
test_attempt$votes <-gsub(pattern = paste(toMatch,collapse="|"), replacement = "", x = test_attempt$votes)
class(test_attempt$votes)
test_attempt$votes <- str_trim(string = test_attempt$votes, side = "both")
test_attempt$votes <- as.integer(test_attempt$votes )

## rating
test_attempt$scores <- gsub(pattern = "Ваша оценка: Нет Рейтинг: ", replacement = "", x = test_attempt$rating, fixed = TRUE)
test_attempt$scores <- gsub(pattern = "\\([^()]+\\)", replacement = "", x = test_attempt$scores)
test_attempt$scores <- str_trim(test_attempt$scores, side = "both")
class(test_attempt$scores)
test_attempt$scores <- as.numeric(test_attempt$scores)

names(test_attempt)
hist(test_attempt$votes)

# views
toMatch_views <- c("просмотр", "просмотров", "просмотра") 
test_attempt$views <-gsub(pattern = paste(toMatch_views,collapse="|"), replacement = "", x = test_attempt$views)
class(test_attempt$views )
test_attempt$views  <- str_trim(string = test_attempt$views , side = "both")
test_attempt$views  <- as.integer(test_attempt$views  )
hist(test_attempt$views)
table(is.na(test_attempt$views))

# correlations
cor(test_attempt$scores, test_attempt$votes, use="complete.obs") # 0.02539876
cor(test_attempt$scores, test_attempt$views, use = "complete.obs") # 0.04970591
cor(test_attempt$votes, test_attempt$views, use = "complete.obs") # 0.8357009

save(test_attempt, file = "data/major_stat_analysis_data.RData")

options(scipen = 999)
model1 <- lm(formula = votes~ scores+ views + fraction*category, data = test_attempt)
summary(model1)




