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
library(scales)

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
loss_product <- ggplot(Product_sales[1:5,], aes(reorder(x=name, -SumProfit), y=SumProfit, fill=name))+
  geom_col(position="dodge")+   
  labs(x='Product loss (Top 5)',y='Loss',title = 'Loss per product')+
  scale_y_continuous(labels = function(x) paste(x/1e3,"k"))+
  coord_flip()+
  guides(fill = "none") +
  geom_text(aes(label = name),
            position = position_stack(vjust = 0.5),
            size=3) +
  theme(axis.text.y=element_blank())
print(loss_product)

#Created a new table for figure of loss-making categories:
Category_sales <- Product_sales %>% 
  select(-id, -name) %>%
  arrange(category,SumProfit) %>% 
  group_by(category) %>%
  summarise(SumProfit = sum(SumProfit)) %>%
  arrange(SumProfit) 

#Draw a piechart for figure of loss-making categories:
loss_category <- ggplot(Category_sales, aes(x="", y=-SumProfit, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(x="", y="", title = 'Profit per category') +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank()) +
  geom_text(aes(label = label_number_si()(SumProfit), hjust = 0.25),
            position = position_stack(vjust = 0.5))
print(loss_category)

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
  guides(fill = "none") +
  geom_text(aes(label = name),
            position = position_stack(vjust = 0.5),
            size=3) +
  theme(axis.text.y=element_blank())
print(late_product)

#Created a new table for figure of late categories:
Category_timeslate <- Product_timeslate %>% 
  select(-id, -name) %>%
  arrange(category,SumLate) %>% 
  group_by(category) %>%
  summarise(SumLate = sum(SumLate)) %>%
  arrange(SumLate) 

#Draw a piechart for figure of late categories:
late_category <- ggplot(Category_timeslate, aes(x="", y=-SumLate, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(x="", y="", title = 'Times late per category') +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank()) +
  geom_text(aes(label = SumLate, hjust = 0.25),
            position = position_stack(vjust = 0.5))
print(late_category)

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
  labs(x='Products returned (Top 5)',y='Times returned per product',title = 'Returns per product')+
  coord_flip()+
  guides(fill = "none") +
  geom_text(aes(label = name),
            position = position_stack(vjust = 0.5),
            size=3)
print(returns_product)

#Created a new table for figure of returned categories:
Category_returns <- Product_returns %>% 
  select(-id, -name) %>%
  arrange(category,SumReturned) %>% 
  group_by(category) %>%
  summarise(SumReturned = sum(SumReturned)) %>%
  arrange(SumReturned) 

#Draw a piechart for figure of returned categories:
returns_category <- ggplot(Category_returns, aes(x="", y=-SumReturned, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(x="", y="", title = 'Times returned per category') +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank()) +
  geom_text(aes(label = SumReturned, hjust = 0.25),
            position = position_stack(vjust = 0.5))
print(returns_category)




#Making a dashboard (install package "patchwork" if not)
library(patchwork)

dashboard <- (loss_product| late_product| returns_product)/
              (loss_category| late_category| returns_category) +
  plot_annotation(
    title = "Profit/loss, times late, and times returned per product & category",
    subtitle = "Assignment 3 of DPV",
    caption = "Made by ChenHao Yi & Marissa Okkerman"
  )
print(dashboard)





