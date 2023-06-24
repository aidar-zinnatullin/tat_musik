
### Насколько различаются текст разных категорий с точки зрения смысла? Подсчитываем число уникальных слов к общему числу слов для каждой песни
### и вычисляем среднее значение для каждой категории.
library(tidyverse)
library(tidytext)
library(here)
library(stringi)
library(formattable)
library(htmltools)
library(writexl)
library(webshot)
webshot::install_phantomjs(force = TRUE)

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

# Load
load("data/major_stat_analysis_data.RData")

### Таблица для категорий песен по количеству просмотров
# categories_desc_views <- test_attempt %>% group_by(category) %>% summarise(n = sum(views)) %>% arrange(desc(n))

categories_desc_views <- test_attempt %>% group_by(category) %>% summarise(n_songs = n(), n = sum(views)) %>% arrange(desc(n_songs))
names(categories_desc_views) <- c("Категория", "Количество песен", "Количество просмотров") 
categ_table <- formattable(categories_desc_views)

export_formattable <- function(f, file, width = "100%", height = "100%", 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}
export_formattable(categ_table, "Figures/Song_Categories_Views.jpeg")

write_xlsx(categories_desc_views, path = "excel_files/Categories_views.xlsx")

# average views

categories_desc_views_mean <- test_attempt %>% group_by(category) %>% summarise(n = sum(views)/n()) %>% arrange(desc(n))
categories_desc_views_mean$n <- round(categories_desc_views_mean$n)
names(categories_desc_views_mean) <- c("Категория", "Среднее количество просмотров")
mean_views_table <- formattable(categories_desc_views_mean)

export_formattable <- function(f, file, width = "100%", height = "100%", 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}
export_formattable(mean_views_table, "Figures/Mean_Categories_Views.jpeg")

write_xlsx(categories_desc_views_mean, path = "excel_files/Mean_categories_views.xlsx")

# Топ 10 песен по количеству просмотров
top_10_songs <- test_attempt %>% select(name, views, category) %>% arrange(desc(views)) %>% slice_head(n = 10)
names(top_10_songs) <- c("Название", "Количество просмотров", "Категория")
formattable_top_10_songs <- formattable(top_10_songs)

export_formattable <- function(f, file, width = "100%", height = "100%", 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}
export_formattable(formattable_top_10_songs, "Figures/Top_10_songs.jpeg")

write_xlsx(top_10_songs, path = "excel_files/Top_10_songs.xlsx")


# votes and scores
top_10_votes <- test_attempt %>% select(name, votes, scores, category) %>% arrange(desc(votes)) %>% slice_head(n = 10)
names(top_10_votes) <- c("Название", "Количество голосов","Оценка", "Категория")
formattable_top_10_votes <- formattable(top_10_votes)

export_formattable <- function(f, file, width = "100%", height = "100%", 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}
export_formattable(formattable_top_10_votes, "Figures/Top_10_votes.jpeg")

write_xlsx(top_10_votes, path = "excel_files/Top_10_votes.xlsx")


### Correlation
names(test_attempt)
ggplot(data = test_attempt, mapping = aes(x= log(views), y =fraction ))+
  geom_point()+ 
  xlab("Просмотры, логарифмированные")+
  ylab("Уникальные слова в песнях")+
  geom_smooth(method='lm')+
  #geom_text(label = all_together_w_o$region_names)+
  theme_classic()
ggsave(filename = 'Figures/Views_Unique_words.jpeg', dpi = 300, width = 14, height = 8)

cor(x = test_attempt$fraction, y = log(test_attempt$views))


### Wordcloud
library(tm)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
docs <- Corpus(VectorSource(test_attempt$preproc))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 

jpeg("Figures/Wordcloud.jpeg", width = 8, height = 8, units = 'in', res = 500)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
dev.off()

