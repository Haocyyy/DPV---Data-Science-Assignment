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

#Created a new table for figure of most loss-making products:
Loss_products <- sales %>% 
  select(productid, profit) %>% 
  rename(id=productid) %>% 
  arrange(id,profit) %>% 
  group_by(id) %>%
  distinct() %>%
  summarise(SumProfit = sum(profit)) %>%
  arrange(SumProfit) %>% 
  full_join(product, by = c("id" = "productid")) %>%
  select( -subcategory)

#Draw a bar graph for figure of most loss-making products:
Loss_product <- ggplot(Loss_products[1:5,], aes(x=reorder(name, -SumProfit), y=SumProfit, fill=name))+
  geom_col(position="dodge")+   
  labs(x='Product loss (Top 5)',y='Loss',title = 'Loss per product')+
  scale_y_continuous(labels = function(x) paste(x/1e3,"k"))+
  coord_flip()+
  guides(fill = "none")

#Created a new table for figure of most late products:
Late_products <- sales %>% 
  select(productid, late) %>% 
  rename(id=productid) %>% 
  arrange(id,late) %>% 
  group_by(id)
  
Late_products$late <- str_replace_all(Late_products$late, "Late", "1")
Late_products$late <- str_replace_all(Late_products$late, "NotLate", "0")
Late_products$late <- as.double(Late_products$late)

Late_products <- Late_products %>%
  na.omit() %>%
  summarise(SumLate = sum(late)) %>%
  arrange(desc(SumLate)) %>%
  left_join(product, by = c("id" = "productid")) %>%
  select( -subcategory)

#Draw a bar graph for figure of most late products:
Late_product <- ggplot(Late_products[1:5,], aes(x=reorder(name, SumLate), y=SumLate, fill=name))+
  geom_col(position="dodge")+   
  labs(x='Late products (Top 5)',y='Shipping time',title = 'Days late per product')+
  coord_flip()+
  guides(fill = "none")




