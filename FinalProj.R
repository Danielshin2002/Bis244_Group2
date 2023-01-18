#install packages
library(tidyverse)


#download datafile
Power <- read.csv("global_power_plant_database.csv")
view(Power)

#save it as a table
fuel_count <- table(Power$primary_fuel)

#save it as a dataframe
fuel_data <- data.frame(primary_fuel=names(fuel_count), count = fuel_count)

#add a column for proportion of fuel type
fuel_data$freq <- round(fuel_data$count.Freq/sum(fuel_data$count.Freq),3)

# arrange the data by frequency
df <- arrange(fuel_data, -freq)
view(df)

#sum to check it is equal to 100% (or 1).
sum(fuel_data$freq)


