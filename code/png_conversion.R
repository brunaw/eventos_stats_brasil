library(pdftools)
library(tidyverse)

arqs <- list.files("img")

files <- arqs %>% 
  paste0("img/", .) %>% 
  purrr::map(pdf_convert, format = "png", 
             dpi = 300, opw = "", upw = "", verbose = TRUE)






