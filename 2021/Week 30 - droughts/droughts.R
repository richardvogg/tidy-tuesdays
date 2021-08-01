#devtools::install_github("ropensci/rnaturalearthhires")

library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(tidyr)


sysfonts::font_add_google(name = "Jura", "Jura")
showtext::showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2021, week = 30)
drought <- tuesdata$drought
rm(tuesdata)

states <- ne_states(country = "united states of america", 
                    returnclass = "sf") %>%
  filter(!iso_3166_2 %in% c("US-AK", "US-HI")) %>% 
  select(state_abb = iso_3166_2, woe_name, geometry) %>%
  mutate(state_abb = substr(state_abb, 4,5))

week <- drought %>%
  mutate(year = format(valid_start, "%Y")) %>%
  filter(drought_lvl == "D2", year != 2001) %>%
  group_by(state_abb, year) %>%
  summarise(area_pct = max(area_pct))
  
states %>%
  right_join(week, by = "state_abb") %>%
  ggplot() + 
  geom_sf(aes(fill = area_pct), colour = NA) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(option = "B") +
  theme_void()

  

#The U.S. Drought Monitor is jointly produced by the National Drought Mitigation Center at the University of Nebraska-Lincoln, the United States Department of Agriculture, and the National Oceanic and Atmospheric Administration. Map courtesy of NDMC.