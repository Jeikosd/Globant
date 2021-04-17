library(readr)
library(dplyr)
library(ggplot2)
library(glue)
library(scales)
library(forcats)


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
  count(`TRANS VAT DESC`) %>% 
  filter(!is.na(`TRANS VAT DESC`)) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  arrange(desc(n))

prop_merchant_name <- clean_data %>% 
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

ggplot()+
  geom_boxplot(data = clean_data, aes(x = "", y = `ORIGINAL GROSS AMT`))+
  theme_bw()+
  labs(y = "Original Gross AMT £")

ggplot(clean_data) +
  aes(x = `ORIGINAL GROSS AMT`) +
  geom_histogram(fill = "#0c4c8a") +
  theme_minimal()

# Estandaricemos los datos de las transacciones para ganar mayor interpretabilidad

clean_data <- clean_data %>% 
  mutate(stand_gross = scale(`ORIGINAL GROSS AMT`))


# Nota no sirvió para un carajo
ggplot()+
  geom_boxplot(data = clean_data, aes(x = "", y = stand_gross))+
  theme_bw()+
  labs(y = "Standarized Gross")

ggplot() +
  geom_line(data = clean_data, aes(x = date, y = `ORIGINAL GROSS AMT`, group = 1))

imputeTS::ggplot_na_distribution(
  x = clean_data$`ORIGINAL GROSS AMT`, 
  x_axis_labels = all_data$date,
  size_points = 1
) 

## Cual es la de mayor recaudo por nombre del comerciante

merchant_name_sum <- clean_data %>% 
  group_by(`MERCHANT NAME`) %>% 
  summarise(sum_gross_amt = sum(`ORIGINAL GROSS AMT`, na.rm = T)) %>% 
  arrange(desc(sum_gross_amt)) %>% 
  left_join(prop_merchant_name, by = "MERCHANT NAME") %>% 
  top_n(n = 20, wt = sum_gross_amt)

ggplot()+
  geom_col(data = merchant_name_sum, aes(x = fct_reorder(`MERCHANT NAME`,
                                                         sum_gross_amt, .desc = F), y = sum_gross_amt))+
  coord_flip()


clean_data %>% 
  group_by(`TRANS CAC DESC 1`) %>% 
  summarise(sum_gross_amt = sum(`ORIGINAL GROSS AMT`, na.rm = T)) %>% 
  arrange(desc(sum_gross_amt)) 

  

