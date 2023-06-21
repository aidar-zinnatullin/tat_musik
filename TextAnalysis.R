##### Text analysis
library(tidyverse)
library(here)
library(tidytext)
library(ggplot2)
load(here("data", "Songs_Text.RData"))
names(more_info_2)[1] <- "text"
write.csv(more_info_2, file = "to_stemm.csv")
## But before, stem / lemmatize on Python
stemmed <-  read_csv("for_to_stemm.csv")

stemmed$rating <- gsub("Ваша оценка: Нет Рейтинг: ", replacement = "", x = stemmed$rating, fixed = TRUE)

# let's merge with composers and writers
load(here("data", "List_of_Songs.RData"))

test_attempt <- left_join(stemmed, songs_website, by = "name")



## categories and words by tf_idf

n_distinct(test_attempt$category)

song_words <- test_attempt %>%
  unnest_tokens(word, preproc) %>% 
  add_count(category, name = "total_words") %>%
  group_by(category, total_words) %>% 
  count(word, sort = TRUE) %>% 
  ungroup()
song_words <- song_words %>% 
  select(-total_words) %>%
  bind_tf_idf(term = word, document = category, n = n)


song_words <- song_words %>% arrange(desc(tf_idf))

facet_bar <- function(df, y, x, by, nrow = 2, ncol = 2, scales = "free") {
  mapping <- aes(y = reorder_within({{ y }}, {{ x }}, {{ by }}), 
                 x = {{ x }}, 
                 fill = {{ by }})
  
  facet <- facet_wrap(vars({{ by }}), 
                      nrow = nrow, 
                      ncol = ncol,
                      scales = scales) 
  
  ggplot(df, mapping = mapping) + 
    geom_col(show.legend = FALSE) + 
    scale_y_reordered() + 
    facet + 
    ylab("")
} 

song_words %>% 
  group_by(category) %>% 
  top_n(15) %>%
  ungroup() %>%
  facet_bar(y = word, 
            x = tf_idf, 
            by = category, 
            nrow = 15)


### I just need to split by 6 categories

# first six categories
first <- unique(song_words$category)[1:6]

first_6 <- song_words[song_words$category %in% first,]
first_6 %>% 
  group_by(category) %>% 
  top_n(15) %>%
  ungroup() %>%
  facet_bar(y = word, 
            x = tf_idf, 
            by = category, 
            nrow = 15)
ggsave(here("Figures", "Figure First 6.jpeg"), width = 10, height = 6, dpi = 300)


# second six categories
second <- unique(song_words$category)[7:12]

second_6 <- song_words[song_words$category %in% second,]
second_6 %>% 
  group_by(category) %>% 
  top_n(15) %>%
  ungroup() %>%
  facet_bar(y = word, 
            x = tf_idf, 
            by = category, 
            nrow = 15)
ggsave(here("Figures", "Figure Second 6.jpeg"), width = 10, height = 6, dpi = 300)

# third six categories
third <- unique(song_words$category)[13:18]

third_6 <- song_words[song_words$category %in% third,]
third_6 %>% 
  group_by(category) %>% 
  top_n(15) %>%
  ungroup() %>%
  facet_bar(y = word, 
            x = tf_idf, 
            by = category, 
            nrow = 15)
ggsave(here("Figures", "Figure Third 6.jpeg"), width = 10, height = 6, dpi = 300)

# fourth six categories
fourth <- unique(song_words$category)[19:24]

fourth_6 <- song_words[song_words$category %in% fourth,]
fourth_6 %>% 
  group_by(category) %>% 
  top_n(15) %>%
  ungroup() %>%
  facet_bar(y = word, 
            x = tf_idf, 
            by = category, 
            nrow = 15)
ggsave(here("Figures", "Figure Fourth 6.jpeg"), width = 10, height = 6, dpi = 300)


# fifth six categories
fifth <- unique(song_words$category)[25:29]

fifth_6 <- song_words[song_words$category %in% fifth,]
fifth_6 %>% 
  group_by(category) %>% 
  top_n(15) %>%
  ungroup() %>%
  facet_bar(y = word, 
            x = tf_idf, 
            by = category, 
            nrow = 15)
ggsave(here("Figures", "Figure Fifth 6.jpeg"), width = 10, height = 6, dpi = 300)




