#libraries to call
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(lubridate)
#import data frame
cycle_202106= data.frame(read.csv("202106_cycle.csv"))
cycle_202107= data.frame(read.csv("202107_cycle.csv"))
cycle_202108= data.frame(read.csv("202107_cycle.csv"))
cycle_202109= data.frame(read.csv("202109_cycle.csv"))
#rbind the data to four the summer
summer_cycle<- rbind(cycle_202106,cycle_202107, cycle_202108)
#change the data types of the dates
as.Date(summer_cycle$started_at, "%Y-%m-%d %H:%M:%S")
as.Date(summer_cycle$ended_at, "%Y-%m-%d %H:%M:%S")
#calculate the ride length
summer_cycle$ride_length <- difftime(summer_cycle$ended_at, summer_cycle$started_at, units = "hours")
summer_cycle$ride_length <- as.numeric(summer_cycle$ride_length)
#choose specific columns to run the analysis
summercycledata <- summer_cycle %>% 
  select(ride_id, rideable_type, member_casual, ride_length)

#change the columns names
summercycledata[summercycledata == "classic_bike"] <- "Classic Bike"
summercycledata[summercycledata == "docked_bike"] <- "Docked Bike"
summercycledata[summercycledata == "electric_bike"] <- "Electric Bike"
#calculate the mean time per membership type
mean_values <- summercycledata %>% 
  group_by(member_casual) %>% 
  summarize(mean(ride_length))

mean_values_bike <- summercycledata %>% 
  group_by(rideable_type) %>% 
  summarize(mean(ride_length))


#plot member casual count for the summer season
ggplot(summercycledata, aes(x = member_casual))+
  geom_bar(aes(fill= member_casual), linewidth = 0.1)+
  facet_wrap(~rideable_type)+
  theme_bw()+
  labs(x = "Membership type", y= "Frequency")+
  guides(fill= "none")+
  theme(text=element_text(size=12, face="bold.italic"))

#plot member casual count for the summer season
ggplot(summercycledata, aes(x = rideable_type))+
  geom_bar(aes(fill= rideable_type), linewidth = 0.1)+
  facet_wrap(~member_casual)+
  theme_bw()+
  labs(x = "Bike Type", y= "Frequency")+
  guides(fill= "none")+
  theme(text=element_text(size=12, face="bold.italic"))
#change date to numric value
summercycledata$ride_length <- as.numeric(summercycledata$ride_length)

#plot member casual count for the summer season
ggplot(summercycledata, aes(x = member_casual, y = ride_length))+
  geom_col(aes(fill=rideable_type))+
  theme_bw()+
    labs(x = "Membership", y= "Hours")+
    theme(text=element_text(size=12, face="bold.italic"))+
  guides(fill=guide_legend(title="Bike Type"))
  
