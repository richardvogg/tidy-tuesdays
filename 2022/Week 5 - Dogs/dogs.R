library(dplyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(widyr)
library(tidyr)
library(ggimage)
library(magick)
library(stringr)
library(purrr)
library(ggchicklet)
library(patchwork)

sysfonts::font_add_google(name = "Cardo", "Cardo")
showtext::showtext_auto()

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')


traits_long <- breed_rank_all %>%
  filter(`2020 Rank` <= 50) %>%
  select(Breed) %>%
  left_join(breed_traits %>% mutate(Breed = str_squish(Breed)), by = "Breed") %>% 
  select(-`Coat Type`, -`Coat Length`) %>%
  pivot_longer(cols = -Breed, names_to = "trait") %>%
  mutate(Breed = as.factor(Breed))

dist_df <- traits_long %>%
  pairwise_dist(Breed, trait, value, upper = TRUE) %>%
  group_by(item1) %>%
  top_n(10, -distance) %>% 
  mutate(rank = rank(distance)) %>%
  ungroup() %>%
  filter((distance <= 3.0) | (rank == 1) ) %>%
  mutate(item1 = as.character(item1), item2 = as.character(item2),
         compare = item1 > item2) %>%
  mutate(help1 = ifelse(compare, item1, item2),
         help2 = ifelse(compare, item2, item1)) %>%
  select(-compare, -item1, -item2, -rank) %>%
  unique() %>%
  select(item1 = help1, item2 = help2, distance)


net <- dist_df %>%
  graph_from_data_frame()

full_df <- data.frame(Breed = names(V(net))) %>%
  left_join(breed_rank_all, by = "Breed")

V(net)$img <- full_df$Image
V(net)$Breed <- full_df$Breed


## function from https://themockup.blog/posts/2021-01-28-removing-image-backgrounds-with-magick/

remove_backgr <- function(breed_name,img_url) {
  # https://themockup.blog/posts/2021-01-28-removing-image-backgrounds-with-magick/
  raw_img <- img_url %>%
    image_read() %>% 
    image_convert("PNG")
  
  img_mask <- raw_img  %>% 
    image_fill("transparent", "+20+20", fuzz = 0, refcolor = "white") %>% 
    image_fill("transparent", "+300+450", fuzz = 1, refcolor = "white") %>%
    image_fill("transparent", "+450+30", fuzz = 1, refcolor = "white") %>%
    image_fill("transparent", "+30+450", fuzz = 1, refcolor = "white") %>%
    image_channel("Opacity") %>%
    image_convert(matte=FALSE) %>%
    image_negate() %>%
    image_median(radius = 10)
  
  # Create Dogs Image Directory if not exists
  image_composite(raw_img, img_mask, operator = "CopyOpacity") %>%
    image_write(paste0("2022/Week 5 - Dogs/img/", breed_name, ".png"))
}


# Remove background and save ----
breed_rank_all %>%
  filter(`2020 Rank` <= 50) %>%
  select(Breed, Image) %>%
  pmap(~remove_backgr(..1, ..2))


set.seed(14)

network_plot <- net %>%
  ggraph(layout="fr") +
  geom_node_point(size = 15, shape = 21, fill = "#DACC96", 
                  col = "#9D5353", stroke = 1) +
  geom_image(aes(x = x, y = y, asp = 1.1,
                 image = paste0("2022/Week 5 - Dogs/img/", Breed, ".png"))) +
  geom_edge_link(aes(edge_alpha = -distance)) +
  geom_node_text(aes(label = Breed), size=3, nudge_y = -0.3, family = "Cardo") +
  labs(title = "Similar traits among popular dog breeds",
       caption = "Data: American Kennel Club") +
  expand_limits(x = -4) +
  theme_graph() +
  theme(plot.background = element_rect(fill = "#BF8B67"),
        legend.position = "none",
        plot.title = element_text(size = 25, family = "Cardo", colour = "#632626"),
        plot.caption = element_text(family = "Cardo", colour = "#632626"))


## Bar charts

bar_chart <- traits_long %>%
  count(trait, value) %>% 
  add_count(trait, wt = value * n) %>%
  ggplot(aes(x = value, y = n)) + 
  geom_col(fill = "#DACC96") +
  facet_wrap(~reorder(trait, nn), ncol = 1) +
  labs(subtitle = "Distribution of values for each trait") +
  theme_void() +
  theme(plot.subtitle = element_text(family = "Cardo", colour = "#632626", size = 18),
        strip.text = element_text(family = "Cardo", colour = "#632626"),
        plot.background = element_rect(fill = "#BF8B67", colour = "#BF8B67"))
  
network_plot + inset_element(bar_chart, 0, 0, 0.2, 1)

ggsave("2022/Week 5 - Dogs/plot.png", width = 11, height = 8)
