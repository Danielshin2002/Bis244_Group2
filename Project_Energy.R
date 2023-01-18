rm(list=ls(all=TRUE))
cat("\014")
##Packages

library(gapminder)
library(here)
library(ggplot2)
library(tidyverse)
library(socviz)
library(readr)
library(dplyr)
install.packages("gridExtra")
library(gridExtra)
library(readr)


##Downloading of Energy Database
url <- "https://wri-dataportal-prod.s3.amazonaws.com/manual/global_power_plant_database_v_1_3.zip"
download.file(url, "EnergyDatabase.zip")
unzip("EnergyDatabase.zip")
Energy_Database <- read.csv("global_power_plant_database.csv")

##Selecting the Values that we need - Country, Megawatt Capacity, Primary Fuel
Energy_Database <- Energy_Database %>%
    select(country, capacity_mw, primary_fuel)



##Plot 1, of types  of the Energy Fuel Source (primary)
p1 <- ggplot(data = Energy_Database, mapping= aes(x = primary_fuel, y = capacity_mw))
p1


p1 + geom_jitter(data = Energy_Database, mapping = aes(x = country, y = capacity_mw, color = primary_fuel))+
  labs(x = "Type of Fuel", 
       y = "MegaWatt Capacity",
       title = "Megawatt Capacity for each type of Power Source, by Country",
       subtitle = "By Group 2",
       caption = "Global Energy Observatory",
       fill = "primary_fuel") +
  facet_wrap(~primary_fuel) + theme_grey() + theme(axis.ticks.x = element_blank(), 
                                                   axis.text.x = element_blank(),
                                                   legend.position = "none")






##Plot 2, comparing the top 38 countries by their Max Megawatt Capacity
Energy_Database2 <-Energy_Database[Energy_Database$capacity_mw > 1000, ]
Energy_Database2 <- Energy_Database2 %>% arrange(desc(capacity_mw))
Energy_Database3 <- Energy_Database2[!duplicated(Energy_Database2$country),]

p3 <- ggplot(Energy_Database3, aes(x = reorder(country, capacity_mw, median, na.rm = TRUE), y = capacity_mw, fill = country))
          
p3 + geom_bar(aes(fill = country, x = reorder(country, capacity_mw, na.rm=TRUE)), stat = "identity", width = 0.7) + 
  labs(y = "Megawatt Capacity",
       x = "Country Abbreviation",
       Title = "Max Megawatt Capacity by Country",
       caption = "Global Energy Observatory") +
  scale_y_continuous(limits = c(0, 25000)) + theme(legend.position = "none") + coord_flip()
          
             
             