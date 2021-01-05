library(tidyverse)
library(tidytext)

tuesdata <- tidytuesdayR::tt_load(2020, week = 39)

peaks <- tuesdata$peaks
members <- tuesdata$members
expeditions <- tuesdata$expeditions



textcol <- "midnightblue"

everest <- expeditions %>%
  filter(peak_id=="EVER") %>%
  mutate(time=cut(year,breaks=c(1900,1960,1970,1980,1990,2000,2010,2020),
                  labels=c("before 1960","1961-1970","1971-1980","1981-1990","1991-2000",
                           "2001-2010","2011-2020"))) %>%
  count(time) %>%
  ggplot(aes(x=time,y=n))+
  geom_col(fill="goldenrod2")+
  labs(title="The Everest Boom",
       subtitle="In recent years 1 out of 4 Himalayan expeditions went to the Everest.",
       y="Number of Everest expeditions")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_text(family = "sans" ,size=14,colour=textcol),
        axis.title.y = element_text(family = "sans" ,size=14,colour=textcol))

pies <- expeditions %>%
  group_by(everest=peak_id=="EVER") %>%
  mutate(time=cut(year,breaks=c(1900,1960,1970,1980,1990,2000,2010,2020),
                  labels=c("before 1960","1961-1970","1971-1980","1981-1990","1991-2000",
                           "2001-2010","2011-2020"))) %>%
  count(time,everest) %>%
ggplot(aes(x = "", y = n, fill = everest )) + 
  geom_bar(stat = "identity", position = position_fill()) +
  #geom_text(aes(label = Cnt), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_grid(.~ time)  +
  scale_fill_manual(values=c("grey70","goldenrod2"))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(family="sans",size=12,colour=textcol)) + 
  labs(caption="Share of Everest expeditions in total Himalayan expeditions")+
  theme(legend.position="none")  


library(patchwork)

everest / pies + plot_layout(heights=c(3,1)) &
  theme(plot.background = element_rect(fill = "ivory"),
        panel.background = element_rect(fill="ivory2"),
        plot.title = element_text(family = "sans", face = "bold", size = 20, colour = textcol),
        plot.subtitle = element_text(family = "sans" ,size=16, colour = textcol),
        plot.caption = element_text(family = "sans" ,size=12, colour = textcol))




####Other ideas####



## Which countries participated in the first climbings?

peaks  %>% 
  unnest_tokens("country","first_ascent_country",token="regex",pattern=", ") %>% 
  mutate(country=str_remove(country,"^w ")) %>%
  count(country,sort=T) 


members %>%
  count(citizenship,everest=peak_id=="EVER",sort=T) %>% 
  filter(everest) %>%
  top_n(10,n) %>%
  ggplot(aes(x=reorder(citizenship,n),y=n))+
  geom_col()+
  coord_flip()+
  labs(title="Top 10 countries with most members on Everest expeditions")

members %>%
  count(citizenship,everest=peak_id=="EVER",sort=T) %>%
  filter(!everest) %>%
  top_n(10,n) %>%
  ggplot(aes(x=reorder(citizenship,n),y=n))+
  geom_col()+
  coord_flip()+
  labs(title="Top 10 countries with most members on non-Everst expeditions")


expeditions %>%
  filter(peak_id=="EVER") %>%
  filter(!str_detect(termination_reason,"Success")) %>%
  ggplot(aes(x=year))+geom_histogram()+facet_wrap(~termination_reason)

