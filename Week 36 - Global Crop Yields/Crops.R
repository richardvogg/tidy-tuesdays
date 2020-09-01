devtools::install_github("rensa/ggflags")

library(tidyverse)
library(ggflags)
#library(ggrepel)

tuesdata <- tidytuesdayR::tt_load(2020, week = 36)


#Fertilizer

fertilizer <- tuesdata$cereal_crop_yield_vs_fertilizer_application %>% 
  filter(!is.na(Code)) %>% 
  select(Entity,Code,Year,fertilizer=`Nitrogen fertilizer use (kilograms per hectare)`,
         crop_yield=`Cereal yield (tonnes per hectare)`)
  
#Get the population from the land use dataset and filter out countries with less than 
#15MM inhabitants
land_use <- tuesdata$land_use_vs_yield_change_in_cereal_production %>% 
  filter(!is.na(Code),!is.na(`Cereal yield index`)) %>% 
  group_by(Entity,Code) %>% 
  summarise(population=last(`Total population (Gapminder)`)) %>% 
  ungroup() %>% 
  filter(population>15000000)

#Calculate the before and after, using the average of two years to be slightly
#more robust against outliers
final <- fertilizer %>% group_by(Entity,Code) %>% 
  summarise(fertilizer=sum(fertilizer,na.rm=T),
            before=max((crop_yield[Year==2001]+crop_yield[Year==2000])/2),
            after=max((crop_yield[Year==2008]+crop_yield[Year==2009])/2)) %>% 
  mutate(perc_change_crop_yield=(after/before)-1) %>% 
  ungroup() %>% 
  filter(!is.nan(perc_change_crop_yield),!is.na(perc_change_crop_yield)) %>% 
  inner_join(land_use,by=c("Code","Entity")) 


#Visualization

textcol <- "midnightblue"

final %>% 
  mutate(code_icons=case_when(Entity=="Chile" ~ "cl",
                            Entity=="Netherlands" ~ "nl",
                            Entity=="Egypt" ~ "eg",
                            Entity=="France" ~ "fr",
                            Entity=="Germany" ~ "de",
                            Entity=="United Kingdom" ~ "gb",
                            Entity=="United States" ~ "us",
                            #Entity=="Peru" ~ "pe",
                            Entity=="South Korea" ~ "kr",
                            Entity=="China" ~ "cn",
                            Entity=="Colombia"~"co",
                            Entity=="Bangladesh"~"bd",
                            Entity=="Japan"~"jp",
                            Entity=="Vietnam"~"vn")) %>%
ggplot(aes(x=fertilizer,y=after))+
  geom_point(size=2)+
  geom_segment(aes(xend=fertilizer,yend=before))+
  geom_flag(aes(country=code_icons),size=8)+
  #geom_text_repel(aes(label=Entity))+
  labs(x="Nitrogen fertilizer use (kg per hectare)",y="Crop yield (tonnes per hectare)",
       title="Fertilizers and their effect on crop yield",
       subtitle="How do crop yields change between 2002 and 2009 depending on the amount of fertilizers used?",
       caption="Data from Our World In Data")+
  theme(plot.background = element_rect(fill = "ivory"),
        panel.background = element_rect(fill="ivory2"),
        axis.title = element_text(family = "sans" ,size=14,colour=textcol),
        axis.text = element_text(family = "sans" ,size=14,colour=textcol),
        plot.title = element_text(family = "sans", face = "bold", size = 20, colour = textcol),
        plot.subtitle = element_text(family = "sans" ,size=16, colour = textcol))





##Other ideas


# The potato map
library(tmap)
data(World)

potatoes <- key_crop_yields %>%
  filter(Year>=2008,!is.na(Code)) %>%
  group_by(Code,Entity) %>% 
  summarise(potato_tph=mean(`Potatoes (tonnes per hectare)`,na.rm=T))

World2 <- World %>% 
  left_join(potatoes,by=c("iso_a3"="Code"))

tm_shape(World2,projection=4326)+
  tm_polygons(col="potato_tph",palette="BuGn")

World2 %>% sf::st_transform(4326) %>% 
ggplot()+geom_sf(aes(fill=potato_tph))


## Corrmorant

library(corrmorant)


potatoes <- key_crop_yields %>%
  filter(Year>=2008,!is.na(Code)) %>%
  group_by(Code,Entity) %>% 
  summarise(potato_tph=mean(`Potatoes (tonnes per hectare)`,na.rm=T))

tractors <- tuesdata$cereal_yields_vs_tractor_inputs_in_agriculture %>%
  filter(!is.na(Code),Year>1980,!is.na(`Tractors per 100 sq km arable land`)) %>%
  group_by(Code,Entity) %>% 
  summarise(tractors=last(`Tractors per 100 sq km arable land`))

World %>% 
  sf::st_drop_geometry() %>% 
  left_join(potatoes,by=c("iso_a3"="Code")) %>% 
  left_join(tractors,by=c("iso_a3"="Code")) %>% 
  select(income_grp,life_exp,potato_tph,tractors) %>% 
  filter(!is.na(life_exp),!is.na(potato_tph),!is.na(tractors)) %>%
  ggcorrm(aes(col=income_grp,fill=income_grp))+
    lotri(geom_point(alpha = 0.5)) +
    utri_corrtext(nrow = 2, squeeze = 0.6) +
    dia_names(y_pos = 0.15, size = 3) +
    dia_density(lower = 0.3, color = 1)


## Chile Profile



country <- "Chile"

key_crop_yields <- tuesdata$key_crop_yields %>% filter(Entity==country)
arable_land <- tuesdata$arable_land_pin %>% filter(Entity==country) %>% 
  rename(arable_land_needed=`Arable land needed to produce a fixed quantity of crops ((1.0 = 1961))`)
fertilizer <- tuesdata$cereal_crop_yield_vs_fertilizer_application %>% filter(Entity==country) %>% 
  rename(nitrogen=`Nitrogen fertilizer use (kilograms per hectare)`,
         yield=`Cereal yield (tonnes per hectare)`)
land_use <- tuesdata$land_use_vs_yield_change_in_cereal_production %>% filter(Entity==country)
tractors <- tuesdata$cereal_yields_vs_tractor_inputs_in_agriculture %>% filter(Entity==country)

#Chile

crop_long <- key_crop_yields %>% 
  pivot_longer(cols=contains("tonnes"),names_to="crop",values_to="tonnes_per_hectar") %>% 
  separate(crop,into = "crop",sep = " ")

library(directlabels)

crop_long %>%
  ggplot(aes(x=Year,y=tonnes_per_hectar,col=crop))+
  geom_line(size=1)+
  geom_dl(aes(label=crop),method="smart.grid")+
  theme(legend.position = "none")

arable_land %>% 
  ggplot(aes(x=Year,
             y=arable_land_needed,
             group=1)) + 
  geom_line()+
  theme(legend.position = "none")

fertilizer %>% filter(Entity%in%c("Chile")) %>% 
  ggplot(aes(x=Year,y=nitrogen,group=Entity))+geom_line()

