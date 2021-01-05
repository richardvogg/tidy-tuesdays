library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2020, week = 49)

shelters <- tuesdata$shelters

full_shelters <- shelters %>%
  group_by(program_name) %>%
  summarise(n=n(),max_cap=max(capacity)) %>%
  filter(n>1000,max_cap>50) %>%
  .$program_name

shelters %>%
  filter(program_name %in% full_shelters) %>%
  ggplot(aes(x=occupancy_date)) +
  geom_line(aes(y=occupancy),col="green")+
  geom_line(aes(y=capacity),col="blue")+
  facet_wrap(~program_name,scales="free_y")

shelters %>%
  group_by(occupancy_date,sector) %>%
  summarise(occupancy=sum(occupancy,na.rm=TRUE),
            capacity=sum(capacity,na.rm=TRUE)) %>%
  ggplot(aes(x=occupancy_date)) +
  geom_line(aes(y=occupancy),col="green")+
  geom_line(aes(y=capacity),col="blue")+
  facet_wrap(~sector,scales="free_y")
  