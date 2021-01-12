library(ggplot2)
library(dplyr)

tuesdata <- tidytuesdayR::tt_load('2021',3)
artwork <- tuesdata$artwork


sysfonts::font_add_google(name = "Nunito","Nunito")
showtext::showtext_auto()


artwork %>%
  filter(width==height,is.na(depth)) %>% View()
  ggplot(aes(xmin=0,xmax=width,ymin=0,ymax=height))+
  geom_rect(fill=NA,col="darkorange")+
  scale_x_log10()+
  scale_y_log10()+
  labs(title="Tate Modern's quadratic art", 
       subtitle = "Height and width in mm (of all quadratic pieces)",
       caption = "Data: Tate Art Museum")+
  theme_light()+
  theme(plot.background = element_rect(fill="steelblue4"),
        panel.background = element_rect(fill="steelblue4"),
    plot.title = element_text(family="Nunito",colour="darkorange",size=20),
        plot.subtitle = element_text(family="Nunito",colour="darkorange",size=15),
    plot.caption = element_text(family="Nunito",colour="darkorange",size=12),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(family="Nunito",colour="darkorange",size=12))
