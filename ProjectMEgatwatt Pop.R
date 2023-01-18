library(gapminder)
library(socviz)
library(tidyverse)
library(ggrepel)
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

url2<-"https://www.census.gov/data-tools/demo/idb/#/table?COUNTRY_YEAR=2023&menu=tableViz"
download.file(url,"export.csv")
Censuspop <- read_csv("export.csv")

Population_only<-data.frame(Censuspop$`Country/Area Name`,Censuspop$Population)

Population_only$country_nm <-tolower(Population_only$Censuspop..Country.Area.Name.)
Energy_Database$country_nm<-tolower(Energy_Database$country_long)

EnergyDB_Pop<- left_join(Population_only,Energy_Database)

BigCountries<-filter(EnergyDB_Pop,Censuspop.Population >100000000)
SmallCountries<- filter(EnergyDB_Pop, Censuspop.Population<10000000)
SmallCountries2.0<-filter(SmallCountries, capacity_mw>2000)

P3<-ggplot(SmallCountries2.0, aes(x=reorder(country_nm,-capacity_mw), y=capacity_mw,size=Censuspop.Population))
P3+geom_bar(aes(fill=country_nm, x=reorder(country_nm, -capacity_mw)), stat="identity", width=0.7)+
  theme(legend.position = "none")+ coord_flip()+ 
  labs(x="Country",y= "Megawatt Capacity",title = "Outputs of Countries \nwith less then 10,000,000 Occupants" )

P4<-ggplot(BigCountries, aes(x=reorder(country_nm,capacity_mw), y=capacity_mw,size=Censuspop.Population))
P4+geom_bar(aes(fill=country_nm, x=reorder(country_nm, -capacity_mw)), stat="identity", width=0.7)+
  theme(legend.position = "none")+ coord_flip()+
  labs(x="Country", y="Megawatt Capacity", title="Outputs of Countries \nmore than 100,000,000 Occupants")+
  scale_y_continuous(limits = c(0,1500000))




Big_SmallCountries2.0<- left_join(BigCountries, SmallCountries2.0)
P4<-ggplot(Big_SmallCountries2.0, aes(x=reorder(country_nm, -capacity_mw),y= capacity_mw))
P4+geom_bar(aes(fill=country_nm,x=reorder(country_nm, -capacity_mw)), stat="identity", width=0.7)+coord_flip()



P3<-ggplot(BigCountries, aes(x=country_nm, y=capacity_mw,size=Censuspop.Population))
P3+geom_jitter(aes(fill=country_nm, x=country_nm))

P3<-ggplot(SmallCountries2.0, aes(x=reorder(country_nm,-capacity_mw), y=capacity_mw,size=Censuspop.Population))
P3+geom_bar(aes(fill=country_nm, x=reorder(country_nm, -capacity_mw)), stat="identity", width=0.7)+theme(legend.position = "none")+ coord_flip()




Big_SmallCountries<-filter(EnergyDB_Pop,Censuspop.Population$ c(Censuspop.Population>100000000, Censuspop.Population<25000000)

aggregate(country_nm ~ capacity_mw, data = NewCountries, mean)

Megawatt_by_Pop2<-ggplot(data=NewCountries, mapping= aes
                        (x=country_nm, y=capacity_mw, size=Censuspop.Population, color= country_nm))
Megawatt_by_Pop2 + geom_bar(data = NewCountries, mapping = aes(x = country_nm, x = capacity_mw, color = country_nm))
 


EnergyDB_Pop<- left_join(Population_only,Energy_Database)

Megawatt_by_Pop<-ggplot(data=EnergyDB_Pop, mapping= aes
                        (x=primary_fuel, y=capacity_mw, size=Censuspop.Population, color= country_nm, 
                          Censuspop.Population> 500000000))
Megawatt_by_Pop + geom_jitter(data = EnergyDB_Pop, mapping = aes(x = country_nm, y = capacity_mw, color = primary_fuel))+ 
  facet_wrap(~primary_fuel) +theme(axis.ticks.x = element_blank(), 
 
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                                                                                                                                                                             axis.text.x = element_blank(),
                                                                                                                                                           legend.position = "none")
Megawatt_by_Pop+geom_point()theme(axis.ticks.x = element_blank(), 
                                  axis.text.x = element_blank(),
                                  legend.position = "none"


View(Mega)

character(Population_only,Energy_Database)
p1<-ggplot(data= worldmap, mapping=aes(x=long, y=lat, group=group))
p1+geom_polygon(size=.1, color="gray90")+guides(fill="none")
