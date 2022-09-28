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
                 options="-c search_path=ass3")

#Get tables from schema ass3
dbGetQuery(con, 
           "SELECT table_name FROM information_schema.tables
           WHERE table_schema='ass3'")

str(dbReadTable(con, c("ass3", "customer")))
str(dbReadTable(con, c("ass3", "product")))
str(dbReadTable(con, c("ass3", "sales")))
str(dbReadTable(con, c("ass3", "returnstatus")))

#Create objects from the database-tables
customer <- dbReadTable(con, c("ass3", "customer"))
product <- dbReadTable(con, c("ass3", "product"))
sales <- dbReadTable(con, c("ass3", "sales"))
returnstatus <- dbReadTable(con, c("ass3", "returnstatus"))

# Visualization -----------------------------------------------------------

library(ggplot2)
library(hrbrthemes)
library(forcats)
library(stringr)

#Created a new table for figure of loss-making products:
Product_sales <- sales %>% 
  select(productid, profit) %>% 
  rename(id=productid) %>% 
  arrange(id,profit) %>% 
  group_by(id) %>%
  summarise(SumProfit = sum(profit)) %>%
  arrange(SumProfit) %>% 
  full_join(product, by = c("id" = "productid")) %>%
  select( -subcategory)

#Draw a bar graph for figure of loss-making products:
loss_product <- ggplot(Product_sales[1:5,], aes(x=reorder(name, -SumProfit), y=SumProfit, fill=name))+
  geom_col(position="dodge")+   
  labs(x='Product loss (Top 5)',y='Loss',title = 'Loss per product')+
  scale_y_continuous(labels = function(x) paste(x/1e3,"k"))+
  coord_flip()+
  guides(fill = "none")
print(loss_product)

#Created a new table for figure of late products:
Product_timeslate <- sales %>% 
  select(productid, late) %>% 
  rename(id=productid) %>% 
  arrange(id,late) %>% 
  group_by(id)
  
Product_timeslate$late <- str_replace_all(Product_timeslate$late, "Late", "1")
Product_timeslate$late <- str_replace_all(Product_timeslate$late, "NotLate", "0")
Product_timeslate$late <- as.double(Product_timeslate$late)

Product_timeslate <- Product_timeslate %>%
  na.omit() %>%
  summarise(SumLate = sum(late)) %>%
  arrange(desc(SumLate)) %>%
  left_join(product, by = c("id" = "productid")) %>%
  select( -subcategory)

#Draw a bar graph for figure of late products:
late_product <- ggplot(Product_timeslate[1:5,], aes(x=reorder(name, SumLate), y=SumLate, fill=name))+
  geom_col(position="dodge")+   
  labs(x='Late products (Top 5)',y='Times late',title = 'Times late per product')+
  coord_flip()+
  guides(fill = "none")
print(late_product)

#Created a new table for figure of returned products:
Product_returns <- sales %>% 
  select(productid, returnstatusid) %>% 
  rename(id=productid) %>% 
  arrange(id,returnstatusid) %>%
  group_by(id) %>%
  summarise(SumReturned = sum(returnstatusid)) %>%
  arrange(desc(SumReturned)) %>% 
  full_join(product, by = c("id" = "productid")) %>%
  select( -subcategory)

#Draw a bar graph for figure of returned products:
returns_product <- ggplot(Product_returns[1:5,], aes(x=reorder(name, SumReturned), y=SumReturned, fill=name))+
  geom_col(position="dodge")+   
  labs(x='Products returned (Top 5)',y='Times returned',title = 'Returns per product')+
  coord_flip()+
  guides(fill = "none")
print(returns_product)





