library(readr)
library(dplyr)
library(ggplot2)
library(glue)


clean_data <-  read_csv(file = glue::glue("data/cleaning/sequence_purchase_transactions.csv"))

clean_data %>% 
  head() %>% 
  as.data.frame()
# Descriptive analysis
# by individual variables
clean_data %>% 
  summarise(avg_gross_amt = mean(`ORIGINAL GROSS AMT`, na.rm = T), 
            coef_variation = sd(`ORIGINAL GROSS AMT`, na.rm = T)/mean(`ORIGINAL GROSS AMT`, na.rm = T)*100)

clean_data %>% 
  count(`MERCHANT NAME`) %>% 
  filter(!is.na(`MERCHANT NAME`)) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  arrange(desc(n))

clean_data %>% 
  count(`TRANS CAC DESC 1`) %>% 
  filter(!is.na(`TRANS CAC DESC 1`)) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  arrange(desc(n))

clean_data %>% 
  count(`TRANS CAC DESC 2`) %>% 
  filter(!is.na(`TRANS CAC DESC 2`)) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  arrange(desc(n))

clean_data %>% 
  count(wday) %>% 
  filter(!is.na(wday)) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  arrange(desc(n))

## Cual es la transaccion destino de mayor cantidad en dolares o libras esterlinas
clean_data %>% 
  group_by(`TRANS CAC DESC 1`) %>% 
  summarise(sum_gross_amt = sum(`ORIGINAL GROSS AMT`, na.rm = T)) %>% 
  arrange(desc(sum_gross_amt)) 

  
ggplot() +
  geom_line(data = all_data, aes(x = date, y = `ORIGINAL GROSS AMT`, group = 1))

ggplot(all_data,aes(date,fill =`ORIGINAL GROSS AMT`)) + geom_tile()

imputeTS::ggplot_na_distribution(
  x = all_data$`ORIGINAL GROSS AMT`, 
  x_axis_labels = all_data$date,
  size_points = 1
) 
