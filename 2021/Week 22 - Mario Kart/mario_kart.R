library(ggplot2)
library(dplyr)
library(ggfx)

tuesdata <- tidytuesdayR::tt_load(2021, week = 22)

drivers <- tuesdata$drivers
records <- tuesdata$records

# Load fonts
sysfonts::font_add_google(name = "Comfortaa", "Comfortaa")
showtext::showtext_auto()


#Tracks, Types and Shortcuts
records %>%
  count(track, type, shortcut)

#Focus on Three Lap Tracks

records %>%
  filter(type == "Three Lap") %>%
  ggplot(aes(x = date, y = time, group = shortcut, col = shortcut)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ track, scales = "free_y")


# Toad's Turnpike

records %>%
  filter(type == "Three Lap", track == "Toad's Turnpike") %>%
  ggplot(aes(x = date, y = time, group = shortcut, col = shortcut)) +
  geom_line(size = 1) +
  geom_point() +
  annotate("text", x = as.Date("2015-01-01"), y = 60, 
           label = "What happened here?", family = "Comfortaa") +
  with_inner_glow(
    annotate("curve", x = as.Date("2015-01-01"), y = 65, 
             xend = as.Date("2020-05-01"), yend = 100, curvature = -0.1)
    ) +
  labs(title = "Toad's Turnpike - Three Lap World records",
       subtitle = glue::glue("Great example of humans being good at optimizing a given metric -
                             but by doing so missing the desired effect.
                             In this case players learned to drive backwards, fall out of the game close to the 
                             finish line and being put back as if the had completed the Lap.
                             Luckily there is still an award for players without taking the shortcut."),
       caption = "Data: Mario Kart World Records") +
  scale_color_manual(values = c("darkred", "goldenrod")) +
  theme_light() +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 15),
        text = element_text(family = "Comfortaa"))


#https://www.reddit.com/r/speedrun/comments/i46qxs/mario_kart_64_new_new_yes_another_one_large_skip/
#https://www.youtube.com/watch?v=uUSymADrVjk