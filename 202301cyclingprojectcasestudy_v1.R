
#libraries to call
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)

#import data frame
cycle_202106= data.frame(read.csv("202106_cycle.csv"))
cycle_202107= data.frame(read.csv("202107_cycle.csv"))
cycle_202108= data.frame(read.csv("202107_cycle.csv"))
cycle_202109= data.frame(read.csv("202109_cycle.csv"))
cycle_202110= data.frame(read.csv("202110_cycle.csv"))
cycle_202111= data.frame(read.csv("202111_cycle.csv"))
cycle_202112= data.frame(read.csv("202112_cycle.csv"))
cycle_202101= data.frame(read.csv("202101_cycle.csv"))
cycle_202102= data.frame(read.csv("202102_cycle.csv"))
cycle_202103= data.frame(read.csv("202103_cycle.csv"))
cycle_202104= data.frame(read.csv("202104_cycle.csv"))
cycle_202105= data.frame(read.csv("202105_cycle.csv"))

#rbind the data to four seasons
summer_cycle= rbind(cycle_202106,cycle_202107, cycle_202108)
fall_cycle = rbind(cycle_202109,cycle_202110, cycle_202111)
winter_cycle = rbind(cycle_202111,cycle_202112, cycle_202101)
spring_cycle = rbind(cycle_202103,cycle_202104, cycle_202105)


#plot member casual count for the summer season
ggplot(summer_cycle, aes(x = member_casual))+
  geom_bar(aes(fill= member_casual), linewidth = 0.1)+
  facet_wrap(~rideable_type)+
  theme_bw()+
  labs(x = "Membership type", y= "Frequency")+
  guides(fill= "none")+
  theme(text=element_text(size=12, face="bold.italic"))

#plot member casual count for the winter season
ggplot(winter_cycle, aes(x = member_casual))+
  geom_bar(aes(fill= member_casual), linewidth = 0.1)+
  facet_wrap(~rideable_type)+
  theme_bw()+
  labs(x = "Membership type", y= "Frequency")+
  guides(fill= "none")+
  theme(text=element_text(size=12, face="bold.italic"))

#plot member casual count for the Fall season
ggplot(fall_cycle, aes(x = member_casual))+
  geom_bar(aes(fill= member_casual), linewidth = 0.1)+
  facet_wrap(~rideable_type)+
  theme_bw()+
  labs(x = "Membership type", y= "Frequency")+
  guides(fill= "none")+
  theme(text=element_text(size=12, face="bold.italic"))
#plot member casual count for the spring season
ggplot(spring_cycle, aes(x = member_casual))+
  geom_bar(aes(fill= member_casual), linewidth = 0.1)+
  facet_wrap(~rideable_type)+
  theme_bw()+
  labs(x = "Membership type", y= "Frequency")+
  guides(fill= "none")+
  theme(text=element_text(size=12, face="bold.italic"))
head(spring_cycle)

#count the number of casual and member customer in each season
spring_cutomers <-spring_cycle%>% count(member_casual) %>% group_by(member_casual)
summer_cutomers <-summer_cycle%>% count(member_casual) %>% group_by(member_casual)
winter_cutomers <-winter_cycle%>% count(member_casual) %>% group_by(member_casual)
fall_cutomers <-fall_cycle%>% count(member_casual) %>% group_by(member_casual)


#Export cleaned data in csv file
springcycle_memb <-spring_cycle %>%   
  select(rideable_type, member_casual)%>%
  write.csv("springcycle_memb.csv")

wintercycle_memb <-winter_cycle %>% 
  select(rideable_type, member_casual)%>%
  write.csv("wintercycle_memb.csv")

fallcycle_memb <-fall_cycle %>% 
  select(rideable_type, member_casual)%>%
  write.csv("fallcycle_memb.csv")

summercycle_memb <-summer_cycle %>% 
  select(rideable_type, member_casual)%>%
  write.csv("summercycle_memb.csv")

