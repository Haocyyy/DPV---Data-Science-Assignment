# Create tables -----------------------------------------------------------


library(readr)
library(dplyr)
library(tidyverse)

#Load the data  
data0 <- read_delim(file = "data/BI_Raw_data.csv",
                    delim = ";", col_names = TRUE, col_types = NULL,
                    locale = locale(encoding="ISO-8859-1"))
head(data0)

#Make Product table 'product':
product <- data0 %>%
  select(Product_Name, Product_Category) %>%
  rename(name = Product_Name, category = Product_Category) %>%
  arrange(name, category) %>%
  group_by(name, category) %>%
  distinct() %>%
  ungroup() %>%
  mutate(productid = row_number())




#Make Customer table
customer <-data0 %>%
  
  select(Customer_Name, Customer_Country) %>%
  rename(name = Customer_Name, country = Customer_Country) %>%
  arrange(name, country) %>%
  group_by(name, country) %>%
  distinct() %>%
  ungroup() %>%
  mutate(customerid = row_number())



#Make sales table

sales <-data0 %>%
  
  select(Order_Date_Day, 
         Product_Name, Product_Category,
         Customer_Name, Customer_Country,
         Order_Price_Total,Product_Order_Price_Total) %>%
  rename(date = Order_Date_Day, 
         product_name = Product_Name, product_category = Product_Category,
         customer_name = Customer_Name, customer_country = Customer_Country,
         order_sales = Order_Price_Total, 
         product_sales = Product_Order_Price_Total) %>%
  arrange(date, product_name, product_category, 
          customer_name, customer_country,
          order_sales,product_sales) 

#Joint product& customer table

sales <- sales %>%
  full_join(product, by = c("product_name" = "name",     "product_category"="category")) %>%
  
  select( -product_name, -product_category) %>% 
  
  full_join(customer, by = c("customer_name" = "name", "customer_country" = "country")) %>% 
  
  select( -customer_name, -customer_country)









# Connect to the PostgreSQL database server -------------------------------

library(DBI)
library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, port = 5432, host = "bronto.ewi.utwente.nl",
                 dbname = "dab_ds22231a_2", user = "dab_ds22231a_2", password = "dzhmetxI0WPAepIp",
                 options="-c search_path=ass2")

product <- as.data.frame(product)
customer <- as.data.frame(customer)
sales <- as.data.frame(sales)
dbWriteTable(con, "product", value = product, overwrite = T, row.names = F)
dbWriteTable(con, "customer", value = customer, overwrite = T, row.names = F)
dbWriteTable(con, "sales", value = sales, overwrite = T, row.names = F)



# Visualization -----------------------------------------------------------

library(ggplot2)
library(hrbrthemes)
library(forcats)
library(stringr)

#Created a new table for figure of best customer
Customer_sales <- sales %>% 
  select(customerid,order_sales) %>% 
  rename(id=customerid,sales=order_sales) %>% 
  group_by(id) %>% 
  summarise(SumSales = sum(sales)) %>% 
  arrange(desc(SumSales)) %>% 
  full_join(customer, by = c("id" = "customerid")) 

#Draw a bar graph
ggplot(Customer_sales[1:5,], aes(x=reorder(name, SumSales), y=SumSales, fill=name))+
  geom_col(position="dodge")+   
  labs(x='Customer (Top 5)',y='Sales',title = 'Sales per customer')+
  scale_y_continuous(labels = function(x) paste(x/1e3,"k"))+
  coord_flip()+
  guides(fill = "none")

#Created a new table for figure of best product
Product_sales <- sales %>% 
  select(productid,product_sales) %>% 
  rename(id=productid,sales=product_sales) %>% 
  group_by(id) %>% 
  summarise(SumSales2 = sum(sales)) %>% 
  arrange(desc(SumSales2)) %>% 
  full_join(product, by = c("id" = "productid")) 

#Draw a bar graph
ggplot(Product_sales[1:5,], aes(x=reorder(name, SumSales2), y=SumSales2, fill=name))+
  geom_col(position="dodge")+   
  labs(x='Product (Top 5)',y='Sales',title = 'Sales per product')+
  scale_y_continuous(labels = function(x) paste(x/1e3,"k"))+
  coord_flip()+
  guides(fill = "none")

#TEST




  

