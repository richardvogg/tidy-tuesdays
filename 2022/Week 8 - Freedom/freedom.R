library(dplyr)
library(ggplot2)
library(tidyr)
library(geomtextpath)
library(patchwork)
library(forcats)


sysfonts::font_add_google(name = "Abel", "Abel")
sysfonts::font_add_google(name = "Fredoka One", "Fredoka")
showtext::showtext_auto()

#load data
freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv') %>%
  mutate(country = ifelse(stringr::str_detect(country, "Ivoire"), "Cote d'Ivoire", country),
         country = ifelse(stringr::str_detect(country, "United Kingdom"), "United Kingdom", country),
         country = ifelse(stringr::str_detect(country, "Democratic People"), "North Korea", country))


freedom_long <- freedom %>%
  pivot_longer(cols = c(PR, CL), names_to = "metric")

spread <- freedom_long %>%
  group_by(country, metric) %>% 
  summarise(max_val = max(value), min_val = min(value)) %>%
  mutate(spread = max_val - min_val)

#select countries with highest change in evaluation
ctrylist <- spread  %>%
  filter(spread > 4) %>%
  .$country %>% unique()


selected_countries <- freedom_long %>%
  filter(country %in% ctrylist, metric == "PR") %>%
  ggplot(aes(x = year, y = 8 - value, group = metric)) +
  geom_line() +
  geom_point(aes(fill = value), size = 3, shape = 21) +
  #geom_textsmooth(data = freedom_long %>% filter(country %in% ctrylist, metric == "PR"),
  #  aes(label = country), method = "loess", formula = y ~ x, span = 0.6,
  #  hjust = "ymax", vjust = -1, family = "Abel", size = 5,
  #  linewidth = 0) +
  geom_text(data = freedom_long %>% filter(country %in% ctrylist, metric == "PR"),
            aes(label = country, x = 2007.5, y = 7.1), family = "Abel", size = 5, 
            fontface = "bold") +
  scale_fill_gradient(low = "#eb6da4", high = "#4c93e7", limits = c(1,7), 
                      labels = c("many", rep("",5) , "few"), guide = "none") +
  facet_wrap(~ factor(country, levels = c("Tunisia", "Sierra Leone", "Mali", "Bhutan",
                                     "Thailand", "Indonesia", "Venezuela (Bolivarian Republic of)")), 
                          dir = "v") +
  expand_limits(y = 8.5) +
  labs(subtitle = "Countries with the highest change") +
  theme_light() +
  theme(strip.text = element_blank(),
        strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        text = element_text(size = 13, family = "Abel"),
        panel.grid.minor = element_blank())



ordered_freedom <- freedom %>%
  add_count(country, wt = PR) %>%
  group_by(country) %>%
  mutate(frst = first(year)) %>%
  filter(frst == 1995)
  

get_plot <- function(region) {
  output_name <- region
  
  g <- ordered_freedom %>%
    filter(Region_Name == region) %>%
    ggplot(aes(x = year, y = reorder(country, n))) +
    geom_tile(aes(fill = PR), col = "grey50") +
    geom_text(data = ordered_freedom %>% 
                filter(Region_Name == region, year == 1995),
              aes(label = country, col = country %in% ctrylist), 
              size = 3, hjust = 0, family = "Abel") +
    scale_fill_gradient(low = "#eb6da4", high = "#4c93e7", limits = c(1,7), 
                        labels = c("many", rep("",5) , "few")) +
    scale_color_manual(values = c("black", "yellow"), guide = "none") +
    labs(subtitle = region, fill = "Political rights") +
    theme_light() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank())
  
  assign(output_name, g)
}

plots <- lapply(unique(freedom$Region_Name), 
  get_plot)


freedom %>%
  count(Region_Name) %>%
  mutate(height = round(n / 140))


layout <- "
AABBCCDDEE
AABBCCDDEE
AABBCCDDEE
AABBCCDD##
AABBCCDD##
AABBCCDD##
AABBCCDD##
AABBCC####
AABB#FFFF#
AA###FFFF#
"



p1 <- plots[[3]] + plots[[1]] + plots[[2]] + plots[[4]] + plots[[5]] + 
  guide_area() +
  annotate("text", x = 0.5, y = 0.1, label = "Title") +
  plot_layout(design = layout, guides = "collect") &
  theme(legend.position = "bottom",
        text = element_text(size = 13, family = "Abel"))

p1 / selected_countries +
  plot_annotation(title = "Freedom in the World - Political rights",
                  caption = "Data: Freedom House, United Nations") &
  theme(plot.title = element_text(size = 25, family = "Fredoka"),
        plot.subtitle = element_text(size = 16, family = "Fredoka"),
        plot.caption = element_text(family = "Abel"))

ggsave("2022/Week 8 - Freedom/plot.png", width = 10, height = 12)

