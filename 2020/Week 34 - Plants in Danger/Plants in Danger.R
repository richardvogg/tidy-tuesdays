install.packages("tidytuesdayR")

library(ggplot2)
library(dplyr)
library(forcats)
library(gghighlight)

tuesdata <- tidytuesdayR::tt_load('2020-08-18')

plants <- tuesdata$plants
threats <- tuesdata$threats
actions <- tuesdata$actions


plants

#Corrplot

plants %>% select(starts_with("threat"),starts_with("action")) %>% cor() %>% corrplot::corrplot()


#What are the biggest threats?

large <- threats %>% 
  group_by(threat_type) %>% 
  summarise(n_species=sum(threatened)) %>% 
  ggplot(aes(x=threat_type,y=n_species))+
  geom_bar(stat="identity",fill="goldenrod3")+
  coord_flip()+
  gghighlight(n_species>50)+
  annotate("text",x=9,y=165,
           label="As these threats have \nfew cases each, we \nmove them to Category 'Other'")+
  annotate(geom = "curve", x = 10.1, y = 170, xend = 10.5, yend = 20, 
    curvature = .1, arrow = arrow(length = unit(2, "mm")))+
  annotate(geom = "curve", x = 7.5, y = 170, xend = 6, yend = 50, 
    curvature = -.1, arrow = arrow(length = unit(2, "mm")))+
  annotate(geom = "curve", x = 7.5, y = 170, xend = 3, yend = 40, 
    curvature = -.3, arrow = arrow(length = unit(2, "mm")))+
  labs(x="",y="Number of extinct species (double count possible)",title="Which are the biggest threats?")

low_freq <- threats %>% group_by(threat_type) %>% 
  summarise(n=sum(threatened)) %>% 
  filter(n<50) %>% .$threat_type

#Continents

continents <- plants %>% 
  group_by(continent) %>% 
  count() %>% 
  ggplot(aes(x=continent,y=n))+
  geom_bar(stat="identity",fill="goldenrod3")+
  coord_flip()+
  gghighlight(n>60)+
  labs(x="",y="Number of extinct species",title="Which are the most affected continents?")

#Per year

years <- plants %>% 
  ggplot(aes(x=fct_relevel(year_last_seen,"Before 1900")))+
  geom_bar()+
  labs(x="Last year the species was seen",y="Number of extinct species")+
  theme(axis.text.x=element_text(angle=20))

#Facets


facets <- threats %>% 
  filter(!is.na(year_last_seen),
         continent%in%c('Africa','North America','South America')) %>% 
  mutate(threat_type=gsub(paste0(low_freq,collapse="|"),"Other",threat_type)) %>% 
  group_by(continent,threat_type,year_last_seen) %>% 
  summarise(n_species=sum(threatened)) %>% 
ggplot(aes(x=fct_relevel(year_last_seen,"Before 1900"),y=n_species))+
  geom_col(fill="goldenrod3")+
  facet_grid(continent~threat_type)+
  gghighlight(n_species>10,calculate_per_facet = T)+
  labs(x="Last year the species was seen (Before 1900 - 2020)",y="")+
  theme(axis.text.x=element_text(angle=90))


#patchwork time

library(patchwork)


(continents| large) /facets + plot_annotation(
  title = 'Plants in danger - Different threats in different continents and times',
  subtitle = 'South America - most species were extinct before 1900.\nAfrica - most species were extinct between 1900-1960\nNorth America - most species were extinct in the last 40 years.'
  
)


#