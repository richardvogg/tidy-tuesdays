library(ggplot2)
library(corrmorant)
library(ggfx)
library(dplyr)
library(stringr)

sysfonts::font_add_google(name = "Fredoka", "Fredoka")
showtext::showtext_auto()

wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')



wheels_num <- wheels %>%
  mutate(construction_cost = as.numeric(str_extract(construction_cost, pattern = '[:digit:]+'))) %>%
  select(Height = height, 
        `Number \n of cabins` = number_of_cabins, 
        `Passengers \n per cabin` = passengers_per_cabin,
        `Seating \n capacity` = seating_capacity,
        `Hourly \n capacity` = hourly_capacity,
        `Duration \n of ride [min]` = ride_duration_minutes,
        `Construction \n cost [MM$]` = construction_cost)

ggcorrm(wheels_num, aes(col = .corr, fill = .corr)) +
  lotri(geom_jitter(alpha = 0.8, width = 0.5, height = 0.5)) +
  with_outer_glow(lotri(geom_smooth(method = "lm", col = "goldenrod2")), 
                  sigma=1) +
  utri_heatmap(alpha = 0.4) +
  utri_corrtext(size = 5) +
  dia_histogram(lower = 0.3, bins = 5, fill = "grey", alpha = 0.8) +
  dia_names(y_pos = 0.3, size = 4) +
  scale_fill_viridis_c(option = "plasma") +
  scale_color_viridis_c(option = "plasma") +
  scale_x_continuous(n.breaks = 4) +
  labs(title = "Ferris wheel metrics",
       subtitle = "Correlations from 73 ferris wheels across the world",
       caption = "Data: @Emil_Hvitfeldt (ferriswheels)") +
  theme(legend.position = "none",
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 20, family = "Fredoka"),
        plot.subtitle = element_text(size = 16, family = "Fredoka"),
        plot.caption = element_text(family = "Fredoka"))


ggsave("2022/Week 31 - FerrisWheels/plot.png", width = 8, height = 8)
