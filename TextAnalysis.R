##### Text analysis
load(here("data", "Songs_Text.RData"))
names(more_info_2)[1] <- "text"
write.csv(more_info_2, file = "to_stemm.csv")
## But before, stem / lemmatize on Python
stemmed <-  read_csv("for_to_stemm.csv")
