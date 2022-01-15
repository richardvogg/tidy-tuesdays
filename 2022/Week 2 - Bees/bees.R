#devtools::install_github("marcosci/layer")

library(layer)
library(dplyr)
library(ggplot2)
library(sf)
library(ggtext)


sysfonts::font_add_google(name = "Nunito", "Nunito")
showtext::showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2022-01-11')
tuesdata <- tidytuesdayR::tt_load(2022, week = 2)

colony <- tuesdata$colony %>%
  mutate(state_low = tolower(state),
         colony_lost_pct = colony_lost_pct/100,
         colony_lost_more10 = case_when(
           colony_lost_pct > 0.3 ~ 0.6,
           colony_lost_pct > 0.15 ~ 0.5,
           TRUE ~ 0.4
         ))



states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  left_join(loss_summ, by = c("ID"="state_low"))

i = 1
for(year_sel in 2015:2021) {
  for(month in unique(colony$months)[1:4]) {
    print(month)
      colony_sel <- colony %>%
        filter(year == year_sel, months == month)
      
      states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
        left_join(colony_sel, by = c("ID"="state_low"))
      
      assign(paste0("tilt_landscape_",i), tilt_map(states, x_shift = floor((i-1)/4) * 100, 
                                                   y_tilt = 1.5, y_shift = 35 * ((i-1)%%4) + floor((i-1)/4) * 10))
      
      i = i+1
  }
  
}



map_list <- list(tilt_landscape_1, tilt_landscape_2, tilt_landscape_3, tilt_landscape_4,
                 tilt_landscape_5, tilt_landscape_6, tilt_landscape_7, tilt_landscape_8,
                 tilt_landscape_9, tilt_landscape_10, tilt_landscape_11, tilt_landscape_12,
                 tilt_landscape_13, tilt_landscape_14, tilt_landscape_15, tilt_landscape_16,
                 tilt_landscape_17, tilt_landscape_18, tilt_landscape_19, tilt_landscape_20,
                 tilt_landscape_21, tilt_landscape_22, tilt_landscape_23, tilt_landscape_24,
                 tilt_landscape_25, tilt_landscape_26)

t <- plot_tiltedmaps(map_list, 
                layer = "colony_lost_more10", begin = rep(0,26), end = rep(1,26),
                palette = "viridis")

t + annotate("text", x = -200, y = 300, label = "Loss of bee swarms in the US", size = 10, hjust = 0, family = "Nunito") +
  annotate("text", x = -230, y = 80, label = "Jan-Mar", size = 7, family = "Nunito")+
  annotate("text", x = -230, y = 115, label = "Apr-Jun", size = 7, family = "Nunito") +
  annotate("text", x = -230, y = 150, label = "Jul-Sep", size = 7, family = "Nunito") +
  annotate("text", x = -230, y = 185, label = "Oct-Dec", size = 7, family = "Nunito") +
  annotate("text", x = -150, y = 230, label = "2015", size = 7, family = "Nunito") +
  annotate("text", x = -50, y = 240, label = "2016", size = 7, family = "Nunito") +
  annotate("text", x = 50, y = 250, label = "2017", size = 7, family = "Nunito") +
  annotate("text", x = 150, y = 260, label = "2018", size = 7, family = "Nunito") +
  annotate("text", x = 250, y = 270, label = "2019", size = 7, family = "Nunito") +
  annotate("text", x = 350, y = 280, label = "2020", size = 7, family = "Nunito") +
  annotate("text", x = 460, y = 220, label = "2021", size = 7, family = "Nunito") +
  geom_richtext(aes(x=50, y = 20, label = "Which states lost<span style='color:#FDE725'> **more than 30%** </span>or<span style='color:#21908C'> **more than 15%** </span><br> of their bee swarms in a quarter?"),
                col = "white", fill = "#440154", size = 6, hjust = 0, 
                label.padding = unit(0.5, "lines"), vjust = 0, family = "Nunito") +
  labs(caption = "Data: USDA via TidyTuesday") +
  theme(plot.caption = element_text(size = 12, family = "Nunito"))


ggsave("2022/Week 1 - OwnData/plot.png", width = 15, height = 9)

