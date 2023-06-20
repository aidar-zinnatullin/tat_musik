library(tidyverse)
library(here)
library(formattable)
library(htmltools)
library(webshot)
webshot::install_phantomjs(force = TRUE)

# Export a Formattable as PNG, PDF, or JPEG
# check: https://github.com/renkun-ken/formattable/issues/26
export_formattable 
load(here("data", "Songs_Text.RData"))
categories_desc <- more_info_1 %>% group_by(category) %>% summarise(n = n()) %>% arrange(desc(n))
names(categories_desc) <- c("Категория", "Количество песен") 
categ_table <- formattable(categories_desc)

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
export_formattable(categ_table, "Figures/Song_Categories.jpeg")
