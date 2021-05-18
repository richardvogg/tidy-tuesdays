devtools::install_github("richardvogg/fuzzymatch")

library(dplyr)
library(ggplot2)
library(stringr)
library(fuzzymatch)
library(patchwork)

#load fonts
sysfonts::font_add_google(name = "Balsamiq Sans","Balsamiq")
showtext::showtext_auto()


#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 21)

survey <- tuesdata$survey


survey <- survey %>%
  mutate(country_dedupes = str_remove_all(country, "[[:punct:]]") %>%
           tolower() %>%
           str_replace("^us|^usa","united states") %>%
           str_replace("^uk","united kingdom") %>%
           str_remove(" of america")) %>%
  mutate(country_dedupes = fuzzy_dedupes(country_dedupes) %>%
           str_to_title())


# Visualization
a <- survey %>% count(country, sort = TRUE) %>%
  slice(1:10) %>%
  mutate(us = c(1,1,1,0,0,0,1,1,1,0)) %>%
  ggplot(aes(x = n, y = reorder(country, n), fill = factor(us))) + 
  geom_col(size = 1, col = "grey20") +
  scale_fill_manual(values = c("grey20", "aquamarine3")) +
  labs(x = "", y = "", subtitle = "Top 10 countries") +
  theme_linedraw() +
  theme(legend.position = "none")
 
b <- survey %>% count(country_dedupes, sort = TRUE) %>%
  slice(1:10) %>%
  mutate(us = c(1, rep(0,9))) %>%
  ggplot(aes(x = n, y = reorder(country_dedupes, n), fill = factor(us))) + 
  geom_col(size = 1, col = "grey20") +
  scale_fill_manual(values = c("grey20", "aquamarine3")) +
  labs(x = "", y = "", subtitle = "With fuzzy deduplication") +
  theme_linedraw() +
  theme(legend.position = "none")

a + b + plot_annotation(title = "Fuzzy deduplication") &
  theme(text = element_text(family = "Balsamiq"),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 18),
        axis.text = element_text(size = 13))
