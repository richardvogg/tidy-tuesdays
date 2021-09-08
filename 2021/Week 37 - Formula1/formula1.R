library(ggplot2)
library(dplyr)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load(2021, week = 37)

sysfonts::font_add_google(name = "Jura", "Jura")
showtext::showtext_auto()

races <- tuesdata$races
results <- tuesdata$results
drivers <- tuesdata$drivers

year_ <- 2012

df_full <- results %>%
  left_join(races, by = "raceId") %>%
  left_join(drivers, by = "driverId") %>%
  select(raceId, driverId, year, position, name, forename, surname,
         points) %>% 
  mutate(position = as.numeric(position),
         position_fill = ifelse(position <= 3, position, NA),
         driver_name = paste(forename, surname),
         name = gsub("Grand Prix", "GP", name)) %>%
  filter(year == year_) %>%
  add_count(driverId, wt = points) 
  

top_10 <- df_full %>%
  distinct(driver_name, n) %>% 
  top_n(10, n)


heatmap <- df_full %>%
  filter(driver_name %in% top_10$driver_name) %>%
ggplot(aes(y = reorder(driver_name, n), x = reorder(name, raceId))) + 
  geom_tile(aes(fill = factor(position_fill)), col = "grey80", size = 1) +
  geom_text(aes(label = position), col = "grey40", family = "Jura") +
  scale_fill_manual(values = c("gold", "cornsilk3", "peru")) +
  labs(title = paste(year_, "Formula 1 Race Results"),
       subtitle = "Top 10 drivers with most points.") +
  theme(legend.position = "none",
        text = element_text(family = "Jura"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 17),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, angle = 18),
        axis.title = element_blank())


bar <- top_10 %>%
  mutate(n_adapted = ifelse(n == max(n), paste(n, "points"), n)) %>%
  ggplot(aes(x = n, y = reorder(driver_name, n))) +
  geom_col(fill = "grey50") +
  geom_text(aes(label = n_adapted), x = 15, col = "white", hjust = 0,
            family = "Jura", size = 5) +
  theme_void()

heatmap + bar + plot_layout(widths = c(0.8, 0.2))

