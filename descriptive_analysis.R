library(readr)
library(dplyr)
library(ggplot2)
library(glue)
library(scales)
library(forcats)
library(naniar)
library(patchwork)
library(stringr)


clean_data <-  read_csv(file = glue::glue("data/cleaning/sequence_purchase_transactions.csv"))

gg_miss_var(clean_data, show_pct = T) + 
  labs(y = "% Missing Values") +
  ggtitle(label = "")

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

# ggplot(clean_data) +
#   aes(x = `ORIGINAL GROSS AMT`) +
#   geom_histogram(fill = "#0c4c8a") +
#   theme_minimal()

# Estandaricemos los datos de las transacciones para ganar mayor interpretabilidad

ggplot() +
  geom_line(data = clean_data, aes(x = date, y = `ORIGINAL GROSS AMT`, group = 1))

imputeTS::ggplot_na_distribution(
  x = clean_data$`ORIGINAL GROSS AMT`, 
  x_axis_labels = clean_data$date,
  size_points = 1
) +
  theme_bw()

## Cual es la de mayor recaudo por nombre del comerciante

merchant_name_sum <- clean_data %>% 
  group_by(`MERCHANT NAME`) %>% 
  summarise(sum_gross_amt = sum(`ORIGINAL GROSS AMT`, na.rm = T)) %>% 
  arrange(desc(sum_gross_amt)) %>% 
  left_join(prop_merchant_name, by = "MERCHANT NAME") %>% 
  top_n(n = 20, wt = sum_gross_amt) %>% 
  mutate(prop = prop/100)

ggplot()+
  geom_col(data = merchant_name_sum, aes(x = fct_reorder(`MERCHANT NAME`,
                                                         sum_gross_amt, .desc = F), y = sum_gross_amt))+
  coord_flip()+
  geom_text(data = merchant_name_sum, aes(x = fct_reorder(`MERCHANT NAME`,
                                                          sum_gross_amt, .desc = F),
                                          label = scales::percent(prop),
                 y= sum_gross_amt), vjust = .5, hjust = -0.1)+
  # ggplot2::lims(y = c(0, 800000))+
  scale_y_continuous(labels = comma, limits = c(0, 8000000))+
  theme_bw()+
  labs(y = "Original Gross AMT £", x = "MERCHANT NAME")

## sum by amazon
prop_merchant_name <- clean_data %>% 
  mutate(condition = str_detect(`MERCHANT NAME`, fixed('amazon', ignore_case=TRUE))) %>% 
  mutate(`MERCHANT NAME` = if_else(condition == TRUE, "amazon", `MERCHANT NAME`)) %>% 
  count(`MERCHANT NAME`) %>% 
  filter(!is.na(`MERCHANT NAME`)) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  arrange(desc(n))

merchant_amazon <- clean_data %>% 
  mutate(condition = str_detect(`MERCHANT NAME`, fixed('amazon', ignore_case=TRUE))) %>% 
  mutate(`MERCHANT NAME` = if_else(condition == TRUE, "amazon", `MERCHANT NAME`)) %>% 
  group_by(`MERCHANT NAME`) %>% 
  summarise(sum_gross_amt = sum(`ORIGINAL GROSS AMT`, na.rm = T)) %>% 
  arrange(desc(sum_gross_amt)) %>% 
  left_join(prop_merchant_name, by = "MERCHANT NAME") %>% 
  top_n(n = 20, wt = sum_gross_amt) %>% 
  mutate(prop = prop/100)

ggplot()+
  geom_col(data = merchant_amazon, aes(x = fct_reorder(`MERCHANT NAME`,
                                                         sum_gross_amt, .desc = F), y = sum_gross_amt))+
  coord_flip()+
  geom_text(data = merchant_amazon, aes(x = fct_reorder(`MERCHANT NAME`,
                                                          sum_gross_amt, .desc = F),
                                          label = scales::percent(round(prop, 3)),
                                          y= sum_gross_amt), vjust = .5, hjust = -0.1)+
  # ggplot2::lims(y = c(0, 800000))+
  scale_y_continuous(labels = comma, limits = c(0, 8000000))+
  theme_bw()+
  labs(y = "Original Gross AMT £", x = "MERCHANT NAME")

clean_data %>% 
  group_by(`TRANS CAC DESC 1`) %>% 
  summarise(sum_gross_amt = sum(`ORIGINAL GROSS AMT`, na.rm = T)) %>% 
  arrange(desc(sum_gross_amt)) 

  

