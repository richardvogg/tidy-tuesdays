library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)

sysfonts::font_add_google(name = "Libre Baskerville", "Libre")
showtext::showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2022-01-18')

chocolate <- tuesdata$chocolate
rm(tuesdata)

avg_df <- chocolate %>%
  summarise(avg_rating = mean(rating), rank = 20) %>%
  mutate(company_location = "total average", .before = 1) %>%
  mutate(col = "a")


set.seed(2)
topflop <- chocolate %>%
  group_by(company_location) %>%
  summarise(avg_rating = mean(rating)) %>%
  mutate(rank = rank(avg_rating, ties.method = "random")) %>% 
  filter((rank > max(rank)-3) | (rank < 4)) %>% 
  mutate(col = "b") %>% 
  rbind(avg_df) %>% 
  ggplot(aes(x = avg_rating, y = reorder(company_location, avg_rating), col = col)) +
  geom_point(size = 10) +
  geom_segment(aes(xend = 0, yend = reorder(company_location, avg_rating)), size = 1) +
  geom_text(aes(label = round(avg_rating,1)), col = "white", family = "Libre") +
  coord_cartesian(xlim = c(2.1, 4), ylim = c(-6, 8)) +
  annotate("text", x = 3.2, y = 2, label = "Really?", size = 12, hjust = 0.5, 
           vjust = 1, family = "Libre") +
  annotate("text", x = 3.2, y = -1, hjust = 0.5,vjust = 1, size = 6, family = "Libre",
    label = glue::glue("Let's take a look at the sample size for each country (below),
                        No country with 10 or more samples has an average under 3 or over 3.4.
                        Extreme results are more likely to happen when sample sizes are smaller.
                       Daniel Kahnemann coined the term 'Law of Small Numbers' for this.")) +
  scale_color_manual(values = c("#a97b4c", "#4d2e07")) +
  labs(subtitle = "Chocolates from Chilean companies are the best rated worldwide",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")
  

small_numbers <- chocolate %>%
  group_by(company_location) %>%
  summarise(n = n(), avg_rating = mean(rating)) %>%
  ggplot(aes(x = avg_rating, y = n)) +
  geom_point(col = "#a97b4c", size = 2) +
  scale_y_log10() +
  geom_text_repel(aes(label = company_location), size = 4, col = "#4d2e07",
                  family = "Libre") +

  coord_cartesian(xlim = c(2.1, 4)) +
  labs(y = "Number of chocolates", x = "Average rating") +
  theme_minimal()

topflop / small_numbers +
  plot_annotation(caption = "Data: Flavors of Cacao") &
  theme(plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        text = element_text(family = "Libre"))

ggsave("2022/Week 3 - Chocolate/plot.png", width = 14, height = 11)
