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

#Make a separate table for returnstatusid
returnstatusid <- data.frame(returnstatusnr = c(0, 1)
                             , returnvalue = c("NotReturned", "Returned")
                             )

#Make Return table 'return' + fulljoin data_returns:
return <- data_main %>%
  select('Order ID') %>%
  rename(orderid = 'Order ID') %>%
  arrange(orderid) %>%
  group_by(orderid) %>%
  distinct() %>%
  ungroup () %>%
  full_join(data_returns, by = c("orderid" = "Order ID")) %>%
  rename(returnvalue = Status) 
  
#Fill in the NoReturns
return[is.na(return)] <- "NotReturned" 

#Find the returnstatusnr with the returnvalue in table return + rownumbers
return <- right_join(return, returnstatusid, by = c("returnvalue" = "returnvalue")) %>%
  mutate (returnstatusid = row_number())




















