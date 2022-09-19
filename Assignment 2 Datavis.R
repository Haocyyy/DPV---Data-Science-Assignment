library(readr)
library(dplyr)
library(tidyverse)

#Connect to SQL database
library(DBI)
library(RPostgreSQL)
library(odbc)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, port = 5432, host = "bronto.ewi.utwente.nl",
                 dbname = "dab_ds22231a_2", user = "dab_ds22231a_2", password = "dzhmetxI0WPAepIp",
                 options="-c search_path=ass2")

#Get tables from schema ass2
dbGetQuery(con, 
           "SELECT table_name FROM information_schema.tables
           WHERE table_schema='ass2'")

str(dbReadTable(con, c("ass2", "customer")))
str(dbReadTable(con, c("ass2", "product")))
str(dbReadTable(con, c("ass2", "sales")))

#Create objects from the database-tables
customer <- dbReadTable(con, c("ass2", "customer"))
product <- dbReadTable(con, c("ass2", "product"))
sales <- dbReadTable(con, c("ass2", "sales"))

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

#Draw a bar graph for figure of best customer
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

#Draw a bar graph for figure of best product
ggplot(Product_sales[1:5,], aes(x=reorder(name, SumSales2), y=SumSales2, fill=name))+
  geom_col(position="dodge")+   
  labs(x='Product (Top 5)',y='Sales',title = 'Sales per product')+
  scale_y_continuous(labels = function(x) paste(x/1e3,"k"))+
  coord_flip()+
  guides(fill = "none")
