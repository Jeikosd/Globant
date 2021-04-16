# unir todos los datos
rm(list = ls())
gc()
gc(reset = T)
library(readr)
library(tibble)
library(stringr)
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

