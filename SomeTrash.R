library(officer)
library(here)
library(tidyverse)
sample_doc <- read_docx(here("тексты татарских песен.docx"))
content <- docx_summary(sample_doc)
content$text[10]
names(content)[4]
write_csv(content, file = "songs_18.csv")

tokenized <- read_csv("msgs_dataset.csv") ### this is gonna be used for the general text analysis

## Then, I use Python to stem
# for_song_18.csv is the result of the stemming

####### Now I have to think about vis
library(quanteda)
library(quanteda.textplots)

tokenized <- read_csv("for_song_18.csv") ### this is gonna be used for the general text analysis
names(tokenized)
tokenized_2 <- tokenized[,c(2,8)]
names(tokenized_2)[2] <- "text"
tokenized_2$text <- str_to_title(tokenized_2$text)
d <- corpus(tokenized_2) %>% 
  tokens() %>% 
  dfm() %>% 
  dfm_toupper()
jpeg("Wordcloud All Words.jpeg", width = 6, height = 6, units = 'in', res = 500)
textplot_wordcloud(d, max_words=100)
dev.off()



##### Collocations
library(forcats)
library(quanteda.textmodels)
library(quanteda.textstats)
kitap_tokens <- corpus(tokenized_2)  %>% 
  tokens(remove_punct=T) %>% 
  tokens_toupper()

colloc_test <-  kitap_tokens %>% 
  textstat_collocations() %>% 
  arrange(desc(`count`)) %>% slice_head(n=20)

colloc_test %>%
  mutate(collocation = fct_reorder(collocation, count)) %>%
  ggplot( aes(x=collocation,y=count)) +
  geom_point() +
  coord_flip() +
  xlab("Сүзтезмә / Словосочетание") +
  ylab("Сан / Количество")+
  theme_bw()

ggsave(filename = "collocations.jpeg", width = 7, height = 7, dpi = 300)


