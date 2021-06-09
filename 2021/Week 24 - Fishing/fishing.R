library(ggplot2)
library(dplyr)
library(sf)
library(patchwork)

#fonts

sysfonts::font_add_google(name = "Jura", "Jura")
showtext::showtext_auto()

#load data

fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')
stocked <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/stocked.csv')


df_logodds <- fishing %>%
  filter(year >= 2000, year <=2008) %>% 
  mutate(grand_total = ifelse(is.na(grand_total), 0, grand_total)) %>%
  group_by(year, lake, species) %>%
  summarise(total_1 = max(grand_total),
            total_2 = max(values, na.rm = TRUE),
            total = round(pmax(total_1, total_2))) %>%
  group_by(lake, species) %>%
  summarise(total = sum(total)) %>%
  ungroup() %>%
  add_count(species, wt = total) %>%
  filter(n > 1000)

#all lakes, all species
df_logodds %>%
  ggplot(aes(x = total, y = reorder(species, total))) +
  geom_segment(aes(xend = 0, yend = reorder(species, total))) +
  geom_point(size = 3) +
  facet_wrap(~ lake, scales = "free_x")


#top 5 species per lake

top_n_plot <- function(lake_name, n_top = 5) {
  df <- df_logodds %>%
    filter(lake == lake_name) %>%
    top_n(n_top, total) %>%
    arrange(total)
  
  df %>%
    ggplot(aes(x = total, y = reorder(species, total))) +
    geom_segment(aes(xend = 0, yend = reorder(species, total))) +
    geom_point() +
    geom_text(aes(x = 0, label = species),
              color = "black", hjust = 0,
              position = position_nudge(y = 0.3),
              fontface = "bold", size = 3, family = "Jura") +
    geom_text(aes(label = scales::comma(total,accuracy = 1)),
              color = "darkblue", hjust = -0.3,
              fontface = "bold", size = 3, family = "Jura") +
    labs(title = paste("Lake", lake_name)) +
    expand_limits(x = max(df$total) * 1.2) +
    theme_void() +
    theme(panel.background = element_rect(color = "black", size = 1))
}

lake_names <- unique(fishing$lake)

lake_top5 <- lapply(lake_names, top_n_plot)


#shapefiles for the Great Lakes from
# http://www.naturalearthdata.com/downloads/10m-physical-vectors/

lakes <- read_sf("C:/Richard/R and Python/Datasets/ne_10m_lakes/ne_10m_lakes.shp") %>%
  filter(name_alt == "Great Lakes", scalerank == 0)


lake_plot <- ggplot(lakes) + geom_sf(fill = "blue", col = "darkblue", size = 1) +
  geom_sf_label(aes(label = name), col = "darkblue", family = "Jura") +
  theme_void() +
  annotate("text", x = -74, y = 45.5, 
           size = 8, family = "Jura",
           label = "Top 5 most fished species \nreported between 2000 and 2008 \nin each one of the Great Lakes") +
  theme(panel.background = element_rect(fill = "khaki3")) +
  expand_limits(x = c(-95, -70), y = c(39, 48))



## Compose the output

lake_plot + 
  inset_element(lake_top5[[1]], 0.52, 0.02, 0.77, 0.25) + #Erie
  inset_element(lake_top5[[2]], 0.75, 0.23, 0.98, 0.48) + #Ontario
  inset_element(lake_top5[[3]], 0.48, 0.73, 0.73, 0.98) + #Huron
  inset_element(lake_top5[[4]], 0.02, 0.5, 0.27, 0.75) + #Superior
  inset_element(lake_top5[[5]], 0.02, 0.24, 0.27, 0.49) + #Michigan
  inset_element(lake_top5[[6]], 0.25, 0.01, 0.5, 0.24) + #Saint Clair
  plot_annotation(caption = "Data: Great Lakes Fishery Commission") &
  theme(text = element_text(family = "Jura"))

