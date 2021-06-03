library(dplyr) # data manipulation
library(ggplot2) # data visualization
library(patchwork) # combining plots
library(ggrepel) # smarter labels
library(ggtext)

sysfonts::font_add_google(name = "Fresca","Fresca")
showtext::showtext_auto()

# Load data

tuesdata <- tidytuesdayR::tt_load(2021, week = 7)

for(name in names(tuesdata)) {
  assign(name,tuesdata[[name]])
}

rm(tuesdata)

# Exploration


df <- income_mean %>%
  filter(income_quintile != "Top 5%",
    dollar_type == "2019 Dollars",
         year %% 10 == 9,
         race %in% c("All Races")) %>%
  tidyr::pivot_wider(id_cols = -c(income_quintile,income_dollars),
                     names_from=income_quintile,
                     values_from=income_dollars) %>%
  mutate(Zero = 0,.before=Lowest) %>%
  tidyr::pivot_longer(cols=Zero:Highest,
                      names_to="income_quintile",
                      values_to="income_dollars") %>%
  mutate(pop_quintile = case_when(
    income_quintile == "Zero" ~ 0,
    income_quintile == "Lowest" ~ 0.2,
    income_quintile == "Second" ~ 0.4,
    income_quintile == "Middle" ~ 0.6,
    income_quintile == "Fourth" ~ 0.8,
    TRUE ~ 1
  )) %>%
  group_by(year,race) %>%
  mutate(income_cumsum=cumsum(income_dollars),
         income_prop = income_cumsum/max(income_cumsum))

df %>%
  ggplot(aes(x=pop_quintile,y=income_prop,col=year,group=year))+
  geom_line(size=1)+
  geom_textbox(aes(x=0.7,y=0.35,label="1969"),
               col="#0072B2", 
               family="Fresca",
               size=5,
               box.colour=NA,
               fill=NA)+
  geom_textbox(aes(x=0.8,y=0.3,label="2019"),
               col="#E69F00",
               family="Fresca",
               size=5,
               box.colour=NA,
               fill=NA)+
  geom_textbox(aes(x=0,y=0.95,label="Increasing inequalities"),
               col="black",
               width=0.5,
               hjust=0,
               size=9,
               family="Fresca")+
  geom_textbox(aes(x=0,y=0.7,
                   label=paste("The Lorenz curve shows how much the distribution of income ",
                                "deviates from an equal distribution (diagonal black line). <br> ",
                                "While in <span style = 'color:#0072B2;'> 1969 </span> the poorer half of the U.S. accounted  ",
                                "for <br> 24% of the income, until <span style = 'color:#E69F00;'> 2019 </span> the value decreased <br> step by ",
                                "step to 18%. <br> In the same time the richest quintile",
                                "increased <br> their income share from 43% to 52%.")),
               width=0.7,
               hjust=0,
               col="black",
               box.colour=NA,
               family="Fresca",
               size=5)+
  geom_segment(aes(x=0,y=0,xend=1,yend=1),col="black")+
  scale_color_gradient(low="#0072B2",high="#E69F00")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),labels=scales::percent)+
  scale_x_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),labels=scales::percent)+
  labs(x="Population proportion",y="Income proportion")+
  theme_light()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_text(family="Fresca",size=12),
        axis.text = element_text(family="Fresca",size=14)) 
  
income_time
income_limits
income_aggregate %>% filter(race=="All Races") %>% View()
