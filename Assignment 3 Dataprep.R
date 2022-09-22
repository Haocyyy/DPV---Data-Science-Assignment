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
  select('Customer Name', 
         Province, 
         Region, 
         'Customer Segment') %>%
  rename(name = 'Customer Name', 
         province = Province, 
         region = Region, 
         segment ='Customer Segment') %>%
  arrange(name, province, region, segment) %>%
  group_by(name, province, region, segment) %>%
  distinct() %>%
  ungroup() %>%
  mutate(customerid = row_number())

#Make Product table 'product':
product <- data_main %>%
  select('Product Name', 
         'Product Category', 
         'Product Sub-Category') %>%
  rename(name = 'Product Name', 
         category = 'Product Category', 
         subcategory = 'Product Sub-Category') %>%
  arrange(name, category, subcategory) %>%
  group_by(name, category, subcategory) %>%
  distinct() %>%
  ungroup() %>%
  mutate(productid = row_number())

#Make a separate table for returnstatusid
returnstatusid <- data.frame(returnstatusid = c(0, 1), 
                             returnvalue = c("NotReturned", "Returned")
                             )

#Make ReturnStatus table 'returnstatus' + fulljoin data_returns:
returnstatus <- data_main %>%
  select('Order ID') %>%
  rename(orderid = 'Order ID') %>%
  arrange(orderid) %>%
  group_by(orderid) %>%
  distinct() %>%
  ungroup () %>%
  full_join(data_returns, by = c("orderid" = "Order ID")) %>%
  rename(returnvalue = Status) 
  
#Fill in the NoReturns
returnstatus[is.na(returnstatus)] <- "NotReturned" 

#Find the returnstatusnr with the returnvalue in table return + rownumbers
returnstatus <- right_join(returnstatus, returnstatusid, by = c("returnvalue" = "returnvalue"))

#Make the inlined dimension Late 'late':
late <- data_main %>%
  select('Order Date', 
         'Ship Date', 
         'Product Name', 'Product Category', 'Product Sub-Category',
         'Customer Name', Province, Region, 'Customer Segment') %>%
  rename(orderdate = 'Order Date',
         shipdate = 'Ship Date',
         product_name = 'Product Name', product_category = 'Product Category', product_subcategory = 'Product Sub-Category',
         customer_name = 'Customer Name', customer_province = Province, customer_region = Region, customer_segment = 'Customer Segment') %>%
  arrange(orderdate, shipdate, product_name, product_category, product_subcategory, customer_name, customer_province, customer_region, customer_segment) %>%
  full_join(product, by = c("product_name" = "name", "product_category"="category", "product_subcategory" = "subcategory")) %>%
  
  select( -product_name, -product_category, -product_subcategory) %>% 
  
  full_join(customer, by = c("customer_name" = "name", "customer_province" = "province", "customer_region" = "region", "customer_segment" = "segment")) %>% 
  
  select( -customer_name, -customer_province, -customer_region, -customer_segment)

#Make a column for Late and NotLate:
library(lubridate)

late$orderdate <- dmy(late$orderdate)
late$shipdate <- dmy(late$shipdate)

late$nrofdays <- interval(late$orderdate, late$shipdate)/ddays()

late$late <- if_else(late$nrofdays > 0, "Late", "NotLate")

#Make the table Sales 'sales':
sales <-data_main %>%
  select('Order Date', 
         'Product Name', 'Product Category', 'Product Sub-Category',
         'Customer Name', Province, Region, 'Customer Segment',
         'Order ID',
         Sales, 'Order Quantity', 'Unit Price', Profit, 'Shipping Cost') %>%
  rename(orderdate = 'Order Date', 
         product_name = 'Product Name', product_category = 'Product Category', product_subcategory = 'Product Sub-Category',
         customer_name = 'Customer Name', customer_province = Province, customer_region = Region, customer_segment = 'Customer Segment', 
         orderid = 'Order ID',
         sales = Sales, orderquantity = 'Order Quantity', unitprice = 'Unit Price', profit = Profit, shippingcost = 'Shipping Cost') %>%
  arrange(orderdate, product_name, product_category, product_subcategory, customer_name, customer_province, customer_region, customer_segment, orderid, sales, orderquantity, unitprice, profit, shippingcost)

sales$orderdate <- dmy(sales$orderdate)

#Join product& customer table:

sales <- sales %>%
  full_join(product, by = c("product_name" = "name", "product_category"="category", "product_subcategory" = "subcategory")) %>%
  
  select( -product_name, -product_category, -product_subcategory) %>%
  
  full_join(customer, by = c("customer_name" = "name", "customer_province" = "province", "customer_region" = "region", "customer_segment" = "segment")) %>% 
  
  select( -customer_name, -customer_province, -customer_region, -customer_segment)

#Join returnstatusid + late (not finished):
sales <- sales %>%
  full_join(returnstatus, by = c("orderid" = "orderid")) %>%
  select ( -returnvalue) 

sales <- sales %>%
  full_join(late, by = c("orderdate" = "orderdate", "productid" = "productid", "customerid" = "customerid")) %>%
  select ( -nrofdays, -shipdate)

#Group and summarize salestable:
sales <- sales %>%
  group_by(productid, customerid, orderdate, returnstatusid, late) %>%
  summarise(sales, profit, orderquantity, shippingcost) %>%
  distinct() %>%
  ungroup() 





















