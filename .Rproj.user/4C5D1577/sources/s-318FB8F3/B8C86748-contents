library(dplyr) # data manipulation
library(ggplot2) # data visualization
library(maps) # city lat and lon
library(patchwork) # combining plots
library(ggrepel) # smarter labels

tuesdata <- tidytuesdayR::tt_load('2021-01-05')

sysfonts::font_add_google(name = "Dosis","Dosis")
showtext::showtext_auto()

df <- tuesdata$transit_cost

df2 <- df %>%
  select(country,city,line,start_year,end_year,length,
         tunnel,stations,real_cost,cost_km_millions) %>% 
  mutate(real_cost=as.numeric(real_cost)) %>% 
  arrange(desc(real_cost)) %>%
  filter(!is.na(country))

#cities from https://simplemaps.com/data/world-cities

cities <- read.csv("C:/Richard/R and Python/Datasets/worldcities.csv") %>%
  group_by(city_ascii) %>%
  mutate(rank=rank(desc(population))) %>%
  filter(rank==1) %>%
  ungroup()


# Does not find a few cities, like Bahrain (2), Xuzhou (4), Tel Aviv (1)
test <- df2 %>%
  mutate(city = ifelse(city=="Seville","Sevilla",city)) %>%
  mutate(country = ifelse(city=="London","GB",country)) %>%
  inner_join(cities,by=c("city"="city_ascii","country"="iso2"))

final <- test %>%
  group_by(city,lng,lat,capital,population) %>%
  summarise(n=n(),
            total_cost=sum(real_cost),
            total_length=sum(length),
            total_stations = sum(stations,na.rm=TRUE),
            total_tunnel = sum(tunnel,na.rm=TRUE),
            earliest=min(start_year,na.rm=TRUE),
            latest=max(end_year,na.rm=TRUE),
            tunnel_per=total_tunnel/total_stations,
            cost_per_km = mean(cost_km_millions,na.rm=TRUE)) %>%
  mutate(total_stations_cat = cut(total_stations,breaks=c(-1,5,20,100,500),
                                  labels=c("<5","6-20","21-100",">100")),
         total_cost_cat = cut(total_cost,breaks=c(-1,5000,20000,50000,5e10),
                              labels=c("<$5B","$5-20B","$20-50B",">$50B")),
         cost_per_km_cat = cut(cost_per_km,breaks=c(-1,100,200,800,1e6),
                               labels=c("<$100M","$100-200M","$200-800M",">$800M")),
         total_length_cat = cut(total_length,breaks=c(-1,20,100,300,1000),
                                labels=c("<20","21-100","100-300",">300")))

a <- ggplot(data=final)+
  borders("world",colour = "grey80")+
  geom_point(aes(x=lng,y=lat,fill=total_stations_cat),shape=21,size=2)+
  geom_text_repel(data=subset(final,
                              (capital=="primary" & population > 5e6) | 
                                population > 10e6 |
                                city %in% c("Sydney","Auckland")),
                  aes(x=lng,y=lat,label=city),min.segment.length = 0,family="Dosis")+
  scale_fill_manual(values=c("white","grey50","lightskyblue","blue"))+
  labs(subtitle="Length in km",fill="Length")+
  coord_cartesian(xlim=c(-120,180),ylim=c(-50,70))+
  theme_light()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


b <- ggplot(data=final)+
  borders("world",colour = "grey80")+
  geom_point(aes(x=lng,y=lat,fill=cost_per_km_cat),shape=21,size=2)+
  geom_text_repel(data=subset(final,
                              (capital=="primary" & population > 5e6) | 
                                population > 10e6 |
                                city %in% c("Sydney","Auckland")),
                  aes(x=lng,y=lat,label=city),min.segment.length = 0,family="Dosis")+
  scale_fill_manual(values=c("white","grey50","indianred2","red"))+
  coord_cartesian(xlim=c(-120,180),ylim=c(-50,70))+
  labs(subtitle="Cost per km",fill="Cost in $MM")+
  theme_light()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())



textcol <- "grey20"

a / b + plot_annotation(title="Transit Costs Project",
                        caption = "Data: Transit Costs Project (transitcosts.com)") &
  theme(plot.background = element_rect(fill = "ivory"),
              panel.background = element_rect(fill="ivory"),
              plot.title = element_text(family = "Dosis", face = "bold", size = 30, colour = textcol),
              plot.subtitle = element_text(family = "Dosis" ,size=20, colour = textcol),
              plot.caption = element_text(family = "Dosis" ,size=12, colour = textcol),
        legend.title = element_text(family="Dosis",colour=textcol,size=14),
        legend.text = element_text(family="Dosis",colour=textcol,size=12),
        legend.background = element_rect(fill="ivory"))
