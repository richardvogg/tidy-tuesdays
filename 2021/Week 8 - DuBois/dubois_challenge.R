library(ggplot2)
library(dplyr)


tuesdata <- tidytuesdayR::tt_load(2021, week = 8)


sysfonts::font_add_google(name = "Quantico","Quantico")
sysfonts::font_add_google(name = "Jura","Jura")
showtext::showtext_auto()

df <- tuesdata$city_rural


t <- seq(6.5, 35.4, by=0.1)
x <-  t * cos(-t)
y <-  t * sin(-t)



ggplot()+
  geom_path(aes(x=x,y=y,size=round(t/10,1)),col="firebrick3") +
  geom_polygon(aes(x=c(-25,-23,38,35),y=c(27,25,83,83),group=1),fill="firebrick3")+
  geom_polygon(aes(x=c(38,35,15,18),y=c(83,83,110,110),group=1),fill="goldenrod1",col="gray50",size=0.01)+
  geom_polygon(aes(x=c(15,18,24,21),y=c(110,110,115,115),group=1),fill="darkcyan")+
  geom_polygon(aes(x=c(24,24,-50,-50),y=c(115,117,117,115),group=1),fill="seagreen4")+
  coord_cartesian(xlim=c(-70,50),ylim=c(-45,160))+
  scale_size_continuous(range=c(1,3.5))+
  annotate("text", x = -10, y = 150,
           label = "CITY AND RURAL POPULATION.\n1890.",
           family = "Quantico", size = 6, lineheight = 0.8) +
  annotate("text", x = -30, y = 112,
           label = "78,139 NEGROES IN CITIES\nOF OVER 10,000 INHABITANTS",
           family = "Jura", size = 3, lineheight = 0.6) +
  annotate("text", x = 37, y = 110,
           label = "8,025 NEGROES IN CITIES\n FROM 5,000 TO 10,000",
           family = "Jura", size = 3, lineheight = 0.6) +
  annotate("text", x = 18, y = 92,
           label = "37,699 \nNEGROES\nIN CITIES\n FROM\n2,500 TO 5,000",
           family = "Jura", size = 3, lineheight = 0.6) +
  annotate("text", x = 0, y = 0,
           label = "734,952",
           family = "Jura", size = 3.5) +
  annotate("text", 0, y = -40,
           label = "NEGROES LIVING IN THE COUNTRY AND VILLAGES",
           family = "Jura", size = 3) +
  theme_void()+
  theme(legend.position="none",
        panel.background = element_rect(fill="blanchedalmond"))
