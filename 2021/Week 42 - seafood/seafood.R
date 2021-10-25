library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtext)
library(rnaturalearth)

options(scipen = 10)

tuesdata <- tidytuesdayR::tt_load(2021, week = 42)

fishery <- tuesdata$`global-fishery-catch-by-sector`

production <- tuesdata$`seafood-and-fish-production-thousand-tonnes`

#clean names
names(production)[4:10] <- strsplit(names(production)[4:10], split = " - ") %>%
  lapply(function(x) x <- x[3]) %>% unlist()

#get country shapefiles
world <- ne_countries(scale = "small", returnclass = 'sf') %>%
  filter(brk_name!="Antarctica") %>%
  select(name,iso_a2,geometry) %>%
  mutate(iso_a2=case_when(
    name=="Norway" ~ "NO",
    name=="France" ~ "FR",
    name=="Namibia" ~ "NA",
    TRUE ~ iso_a2
  )
)



prod_long <- production %>%
  pivot_longer(cols = 4:10, names_to = "type", values_to = "tonnes")

prod_long %>%
  filter(Entity %in% c("World")) %>%
  ggplot(aes(x = Year, y = tonnes, fill = type, group = type)) +
  geom_stream(type = 'proportional')

top1_per_country <- prod_long %>%
  filter(type %in% c("Pelagic Fish", "Demersal Fish", "Freshwater Fish")) %>%
  mutate(countrycode = countrycode::countryname(Entity, destination = "iso2c")) %>% 
  filter(!is.na(countrycode)) %>% 
  group_by(countrycode, type) %>%
  summarise(tonnes = sum(tonnes, na.rm = TRUE)) %>%
  group_by(countrycode) %>%
  top_n(1, tonnes)


colors <- c("#F44747", "#EEDC31", "#0E68CE")

world %>%
  left_join(top1_per_country, by = c("iso_a2" = "countrycode")) %>%
  ggplot() +
  geom_sf(aes(fill = type, alpha = pmin(tonnes, 5e7), col = type)) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors, na.value = "grey80") +
  labs(title = glue::glue("Are countries mainly fishing 
                          <span style='color:#EEDC31;'>Freshwater</span>, 
                          <span style='color:#F44747;'>Demersal</span>
                          or <span style='color:#0E68CE;'>Pelagic</span> Fish?"),
       subtitle = "Transparency shows amount of mainly fished fish per country.",
       caption = "Data: OurWorldInData") +
  theme(plot.title = element_markdown(size = 20),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.text = element_blank(),
        axis.ticks = element_blank())
