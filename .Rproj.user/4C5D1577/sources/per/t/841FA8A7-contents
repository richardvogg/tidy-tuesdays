library(ggplot2)
library(dplyr)
library(rKenyaCensus)
library(tmap) # World map shapes
library(sf) # Plot maps with ggplot
library(geogrid) # Convert shapefile into hexagon shapefile
library(stringr) # String operations (replace characters, remove characters)
library(patchwork)

#Add fonts from google
sysfonts::font_add_google(name = "Fresca","Fresca")
showtext::showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2021',4)

households <- tuesdata$households %>%
  mutate(County= County %>% toupper() %>% trimws())


births <- rKenyaCensus::V4_T2.37 %>%
  mutate(County = str_replace(County,"/","-"))

data("World") # from the tmap package

kenya_in_africa_map <- World %>% 
  filter(continent == "Africa") %>%
  mutate(kenya = ifelse(name=="Kenya","Y","N")) %>%
  ggplot()+geom_sf(aes(fill=kenya),size=NA)+
  scale_fill_manual(values=c("grey80","blue"),guide=FALSE)+
  theme_void()+
  theme(panel.background = element_rect(colour="black"))
  

kenya_shapes <- rKenyaCensus::KenyaCounties_SHP %>%
  st_as_sf() %>%
  st_simplify(dTolerance=500) %>% # makes the shapefile lighter and much faster to load
  mutate(County_for_join=str_replace(County,"/","-") %>%
           str_replace("HOMA BAY","HOMABAY"))


kenya <- kenya_shapes %>%
  left_join(births,by=c("County_for_join"="County")) %>% 
  ggplot()+geom_sf(aes(fill=Percent_in_HealthFacility),col=NA)+
  scale_fill_gradient(low="grey80",high="darkorange",guide=FALSE)+
  theme_void()+
  theme(panel.background = element_rect(colour="black"))

#Use patchwork to put maps together
inset_map <- kenya_in_africa_map + kenya

#Hex format

new_cells_hex <- calculate_grid(shape = kenya_shapes, grid_type = "hexagonal", seed = 5)
kenya_hex_shapes <- assign_polygons(kenya_shapes, new_cells_hex)

kenya_hex <- kenya_hex_shapes %>% 
  left_join(births,by=c("County_for_join"="County")) %>%
  mutate(Population=as.numeric(as.character(Population))) %>% 
  mutate(County=County %>% str_replace("/","/\n") %>%
           str_replace(" ","\n") %>%
           str_replace("-","-\n")) %>%
  ggplot()+geom_sf(aes(fill=Percent_in_HealthFacility))+
  geom_sf_text(aes(label=County),size=2)+
  scale_fill_gradient(low="grey80",high="darkorange")+
  labs(title="Kenya's counties and births in Health Facilities",
       subtitle="Counting births in the 12 months preceeding the census 2019", 
       caption="Data: github.com/Shelmith-Kariuki/rKenyaCensus",
       fill = "% Births in \nHealth Facilities")+
  theme_void()+
  theme(legend.position = c(0.2,0.15),
        plot.title = element_text(size=20,family="Fresca"),
        plot.subtitle = element_text(size=16,family = "Fresca"))



kenya_hex + 
  inset_element(inset_map,left=0.6,bottom = 0.8,right=0.99,top=0.99)
