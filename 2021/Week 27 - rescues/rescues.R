library(ggplot2)
library(dplyr)
library(lubridate)
library(forcats)
library(stringr)
library(patchwork)
library(viridisLite)

sysfonts::font_add_google(name = "Jura", "Jura")
showtext::showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2021, week = 27)
animals <- tuesdata$animal_rescues
rm(tuesdata)


### Top 10 most expensive animal rescues

animals %>%
  mutate(incident_notional_cost  = as.numeric(incident_notional_cost)) %>%
  top_n(10, incident_notional_cost) %>% 
  ggplot(aes(x = incident_notional_cost, 
             y = reorder(date_time_of_call, incident_notional_cost),
             fill = animal_group_parent, col = animal_group_parent)) +
  geom_col(alpha = 0.3, size = 3) +
  geom_text(aes(x = 35, 
                label = final_description %>%
                  str_wrap(50)), 
            hjust = 0, col = "black", family = "Jura") +
  labs(x = "cost", y = "", title = "Most expensive animal rescues",
       caption = "Data: London Fire Brigade") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_light() + 
  theme(legend.position = c(0.8, 0.2),
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        text = element_text(family = "Jura"))



#### Other ideas





df <- animals %>%
  select(incident_number, date_time_of_call, incident_notional_cost, 
         animal_group_parent, special_service_type_category) %>%
  mutate(datetime = dmy_hm(date_time_of_call),
         year = year(datetime),
         month = month(datetime, label = TRUE),
         day = day(datetime),
         hour = hour(datetime),
         animal_group_parent = tolower(animal_group_parent))


plots <- lapply(c("cat", "dog"), function(x) {
  lapply(c("year", "month", "day", "hour"), function(y) {
    df %>%
      filter(animal_group_parent == x) %>%
      ggplot(aes(x = eval(parse(text = y)), 
                 fill = special_service_type_category,
                 alpha = 0.2)) + 
      geom_density(adjust = 1/2)
  })
}) %>% do.call(rbind, .)

plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] +
  plots[[5]] + plots[[6]] + plots[[7]] + plots[[8]] +
  plot_layout(ncol = 2, guides = "collect")


plots[[7]] +
  coord_polar(theta = "x")

