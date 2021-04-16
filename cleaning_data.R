library(fs)
library(here)
library(dplyr)
library(rvest)
library(stringr)
library(purrr)
library(glue)
library(open)
# "Spanish_Colombia.1252"
Sys.setlocale("LC_TIME", "English")
# dowload files

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

# cleaning name files
# cut-off date on the third day

library(readxl)
library(lubridate)
datos <- read_xls("data/cusersfinainmndesktoppublish-copy-january-2017.xls")
datos <- read_xls("data/cusersfinainmndesktoppublish-spend-april-2017.xls")
datos <- datos %>% 
  mutate(year = lubridate::year(`TRANS DATE`), 
         month = lubridate::month(`TRANS DATE`),
         day = lubridate::day(`TRANS DATE`),
         wday = lubridate::wday(`TRANS DATE`, label = T, abbr = F))
datos %>% 
  dplyr::filter(year == 2016) %>% 
  as.data.frame()

datos %>% 
  dplyr::filter(month == 2) %>% 
  as.data.frame()

datos %>% 
  dplyr::select(year, month) %>% 
  distinct(year, month)

datos %>% 
  dplyr::select(year, month, day, wday) %>% 
  distinct(year, month, day, wday) %>% 
  dplyr::filter(month == 2)

# purchase card transition
## Change to PCT_month_year
# Example PCT_01_2016  -> purchase card transition january 2016
datos <- read_xls("data/cusersfinainmndesktoppublish-copy-january-2017.xls")
datos <- datos %>% 
  mutate(year = lubridate::year(`TRANS DATE`), 
         month = lubridate::month(`TRANS DATE`),
         day = lubridate::day(`TRANS DATE`),
         wday = lubridate::wday(`TRANS DATE`, label = T))
