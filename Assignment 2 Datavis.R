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
  select(customerid,sales) %>% 
  rename(id=customerid) %>% 
  arrange(id,sales) %>% 
  group_by(id) %>% 
  distinct() %>%
  summarise(SumSales = sum(sales)) %>% 
  arrange(desc(SumSales)) %>% 
  full_join(customer, by = c("id" = "customerid")) %>%
  select( -country)

#Draw a bar graph for figure of best customer
best_customer <- ggplot(Customer_sales[1:5,], aes(x=reorder(name, SumSales), y=SumSales, fill=name))+
  geom_col(position="dodge")+   
  labs(x='Customer (Top 5)',y='Sales',title = 'Sales per customer')+
  scale_y_continuous(labels = function(x) paste(x/1e3,"k"))+
  coord_flip()+
  guides(fill = "none")
  print(best_customer)

#Created a new table for figure of best product
Product_sales <- sales %>% 
  select(productid,sales) %>% 
  rename(id=productid) %>% 
  arrange(id,sales) %>% 
  group_by(id) %>%
  distinct() %>%
  summarise(SumSales2 = sum(sales)) %>% 
  arrange(desc(SumSales2)) %>% 
  full_join(product, by = c("id" = "productid")) %>%
  select( -category)

#Draw a bar graph for figure of best product
best_product <- ggplot(Product_sales[1:5,], aes(x=reorder(name, SumSales2), y=SumSales2, fill=name))+
  geom_col(position="dodge")+   
  labs(x='Product (Top 5)',y='Sales',title = 'Sales per product')+
  scale_y_continuous(labels = function(x) paste(x/1e3,"k"))+
  coord_flip()+
  guides(fill = "none")
  print(best_product)



#Making a dashboard (install package "patchwork" if not)
library(patchwork)

dashboard <- (best_customer| best_product) +
  plot_annotation(
    title = "Best customers and best products",
    subtitle = "Assignment 2 of DPV",
    caption = "Made by ChenHao Yi & Marissa Okkerman"
  )
print(dashboard)

