library(tidyverse)
library(here)

load(here("data", "Songs_Text.RData"))
categories_desc <- more_info_1 %>% group_by(category) %>% summarise(n = n()) %>% arrange(desc(n))
