# unir todos los datos
rm(list = ls())
gc()
gc(reset = T)
library(readr)
library(tibble)
library(stringr)
library(dplyr)
library(glue)


purchase_transactions <- list.files(path = "data/cleaning/", pattern = "PCT", full.names = T) %>% 
  tibble(file_path = .) %>%
  mutate(file = basename(file_path),
         year = as.integer(stringr::str_sub(file, start = 5, end = 8)),
         month = as.integer(stringr::str_sub(file, start = 10, end = 11))) %>% 
  arrange(year, month) %>% 
  dplyr::select(file_path) %>% 
  mutate(data = purrr::map(.x = file_path, function(x){
    read_csv(x)
  })) %>% 
  pull(data) %>% 
  bind_rows()

## Make a daily sequence to identify 
init_last <- purchase_transactions %>% 
  filter(row_number()==1 | row_number()==n()) %>% 
  dplyr::select(`TRANS DATE`) %>% 
  pull()

sequence_date <- seq.Date(from = init_last[1], to = init_last[2], by = "1 day") %>% 
  tibble(date = .)

all_data <- full_join(sequence_date, purchase_transactions, by = c("date" = "TRANS DATE"))

write_csv(all_data, file = glue::glue("data/cleaning/sequence_purchase_transactions.csv"))














