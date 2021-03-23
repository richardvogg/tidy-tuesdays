library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)


tuesdata <- tidytuesdayR::tt_load(2021, week = 13)

sysfonts::font_add_google(name = "Dosis","Dosis")
showtext::showtext_auto()

unvotes <- tuesdata$unvotes %>%
  mutate(country_code=ifelse(country=="Namibia","NA",country_code))
roll_calls <- tuesdata$roll_calls
issues <- tuesdata$issues

world <- ne_countries(scale = "small", returnclass = 'sf') %>%
  filter(brk_name!="Antarctica") %>%
  select(name,iso_a2,geometry) %>%
  mutate(iso_a2=case_when(
    name=="Norway" ~ "NO",
    name=="France" ~ "FR",
    name=="Namibia" ~ "NA",
    TRUE ~ iso_a2
    )
  )


votes <- roll_calls %>%
  filter(date>as.Date("2010-01-01"))

unvotes_decade <- unvotes %>%
  filter(rcid %in% votes$rcid) %>%
  left_join(issues,by="rcid") %>%
  group_by(country,country_code,issue) %>%
  summarise(perc_yes = sum(vote=="yes",na.rm=TRUE)/n(),
            perc_no = sum(vote=="no",na.rm=TRUE)/n()) %>%
  filter(!is.na(issue))

world %>% 
  right_join(unvotes_decade,by=c("iso_a2"="country_code")) %>%
  ggplot()+
  geom_sf(aes(fill=perc_yes),col=NA)+
  scale_fill_continuous(type="viridis",labels=scales::percent)+
  #scale_fill_binned(type="viridis",
  #                labels=c("less than 25%","","more than 75%"))+
  facet_wrap(~issue)+
  labs(title="Who votes YES for resolutions and amendments?",
       subtitle="Percentage of YES votes per country in the UN General Assembly \nbetween 2010 and 2019.",
       fill = "% of YES votes",
       caption = "Data: Harvard Dataverse")+
  theme_light()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=18,family="Dosis",color="#440154FF"),
        plot.subtitle = element_text(size=16,family="Dosis",color="#440154FF"),
        strip.background = element_rect(fill="#FDE725FF"),
        strip.text = element_text(color="#440154FF",size=12,family="Dosis"),
        plot.caption = element_text(size=10,family="Dosis",color="#440154FF"))




### Other ideas

library(tidytext)

data(stop_words)

stop_words <- stop_words %>%
  add_row(word=c("human","rights","right"))



conflict_areas <- c("palestine","israel","iran","iraq","chile","sudan","syria")

freq_words <- roll_calls %>%
  full_join(issues,by="rcid") %>%
  filter(short_name=="hr") %>%
  unnest_tokens(word,short) %>% 
  anti_join(stop_words) %>%
  mutate(year=as.numeric(format(date,"%Y")),
         year2=cut(year,breaks=c(1946,1960,1980,2000,2020),
                   labels=c("1940-1960","1960-1980","1980-2000","2000-2019"))) %>%
  group_by(year2) %>%
  count(word,sort=TRUE) %>% 
  filter(n>2)



