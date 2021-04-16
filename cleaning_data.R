library(fs)
library(here)
library(dplyr)
library(rvest)
library(stringr)
library(purrr)
library(glue)
library(readxl)
library(lubridate)
library(readr)
library(tidyr)

## dir to save the purchase card transactions
fs::dir_create("data/")
fs::dir_create("data/cleaning")

# "Spanish_Colombia.1252" my locale
Sys.setlocale("LC_TIME", "English")
# download files

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
# purchase card transactions
## Change to PCT_month_year
# Example PCT_01_2016  -> purchase card transition january 2016
cleaning_name <- function(x, out_dir){
  
  # datos <- read_xls("data/cusersfinainmndesktoppublish-spend-sept-2017.xls")
  # datos <- read_xls("data/cusersfinainmndesktoppublish-spend-sept-2017.xls", .name_repair = "minimal")
  
  datos <- read_xls(x)
  # unique_columns <- unique(colnames(datos))
  print(x)
  datos <- datos %>% 
    # dplyr::select(!!unique_columns) %>% 
    mutate(year = lubridate::year(`TRANS DATE`), 
           month = lubridate::month(`TRANS DATE`),
           day = lubridate::day(`TRANS DATE`),
           wday = lubridate::wday(`TRANS DATE`, label = T))
  
  # Selecting the month with the most data to generate the file name
  name_purchase_card <- datos %>% 
    dplyr::count(year, month) %>% 
    top_n(1, n) %>% 
    mutate(month = sprintf("%.2d", month),
           name_purchase_card = glue::glue("PCT_{year}_{month}.csv")) %>% 
    pull(name_purchase_card)
  
  number_columns <- length(colnames(datos))
  
  verify_data <- tibble::tibble( file = "data/cusersfinainmndesktoppublish-spend-sept-2017.xls",
                 columns = colnames(datos)) %>% 
    # mutate(ID = 1) %>%
    tidyr::pivot_wider(id_cols = file, names_from = columns, values_from = columns) %>% 
    mutate(number_columns = number_columns)
    
  
 
  # write_csv(x = datos, file = glue::glue("{out_dir}/{name_purchase_card}"))

  return(verify_data)
}
xls_files <- list.files(path = "data/", pattern = "*.xls", full.names = T)
check_columns <- purrr::map(.x = xls_files, .f = cleaning_name, out_dir = "data/cleaning/")
check_columns <- check_columns %>% 
  bind_rows()
write_csv(check_columns, "prueba.csv")

# cut-off date on the third day

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


