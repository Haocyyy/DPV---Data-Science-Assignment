# Create tables -----------------------------------------------------------

library(readr)
library(dplyr)
library(tidyverse)

#Load the data  
data_main <- read_delim(file = "data/SuperstoreSales_main.csv",
                    delim = ";", col_names = TRUE, col_types = NULL,
                    locale = locale(encoding="ISO-8859-1"))
head(data_main)

data_manager <- read_delim(file = "data/SuperstoreSales_manager.csv",
                        delim = ";", col_names = TRUE, col_types = NULL,
                        locale = locale(encoding="ISO-8859-1"))
head(data_manager)

data_returns <- read_delim(file = "data/SuperstoreSales_returns.csv",
                        delim = ";", col_names = TRUE, col_types = NULL,
                        locale = locale(encoding="ISO-8859-1"))
head(data_returns)

#Make Customer table 'customer':
customer <- data_main %>%
  select('Customer Name', Province, Region, 'Customer Segment') %>%
  rename(name = 'Customer Name', province = Province, region = Region, segment ='Customer Segment') %>%
  arrange(name, province, region, segment) %>%
  group_by(name, province, region, segment) %>%
  distinct() %>%
  ungroup() %>%
  mutate(customerid = row_number())

#Make Product table 'product':
product <- data_main %>%
  select('Product Name', 'Product Category', 'Product Sub-Category') %>%
  rename(name = 'Product Name', category = 'Product Category', subcategory = 'Product Sub-Category') %>%
  arrange(name, category, subcategory) %>%
  group_by(name, category, subcategory) %>%
  distinct() %>%
  ungroup() %>%
  mutate(productid = row_number())

returnstatusid <- data.frame(nr = c(0, 1)
                             , label = c("Returned", "NotReturned")
                             )











#Make Return table 'return':

return <- data_main %>%
  select('Order ID') %>%
  rename(orderid = 'Order ID') %>%
  arrange(orderid) %>%
  group_by(orderid) %>%
  distinct() %>%
  ungroup () %>%
  full_join(data_returns, by = c("orderid" = "Order ID")) %>%
  mutate(returnstatusid = row_number()) 

return[is.na(return)] <- 'NotReturned'













