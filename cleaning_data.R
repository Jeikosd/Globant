library(fs)
library(here)
library(dplyr)
library(rvest)
library(stringr)
library(purrr)
library(glue)


webpage_url <- "https://data.birmingham.gov.uk/dataset/purchase-card-transactions"
webpage <- rvest::read_html(webpage_url)

purchase_card_1 <- webpage %>% 
  html_nodes("a") %>%  
  html_attr("href") %>%
  str_subset("\\.xls") 

purchase_card_2 <- webpage %>% 
  html_nodes("a") %>%  
  html_attr("href") %>%
  str_subset("\\.csv") 

purrr::map(.x = purchase_card_1, .f = function(x){
  names_ <- basename(x)
  download.file(x, glue::glue("data/{names_}"), mode = "wb")
})

purrr::map(.x = purchase_card_2, .f = function(x){
  names_ <- basename(x)
  download.file(x, glue::glue("data/{names_}"), mode = "wb")
})

