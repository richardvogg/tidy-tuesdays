remotes::install_github("r-link/corrmorant")

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(patchwork)
library(ggimage)
library(corrrmorant)

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')


# Which is the best metric to measure how cool a game is?
# Bayes_average better than average
# Using owned, wanted/wished from the details df

bayes <- ratings %>%
  ggplot(aes(x = bayes_average, y = users_rated)) + geom_point()

normal <- ratings %>%
  ggplot(aes(x = average, y = users_rated)) + geom_point()

bayes + normal




# Most popular boardgamepublisher

details %>%
  mutate(publisher = boardgamepublisher %>%
           strsplit(split = ",") %>% 
           lapply(function(x) x <- x[1]) %>%
           unlist()) %>% 
  mutate(publisher = publisher %>% 
           str_remove("\\[\\'") %>%
           str_remove("'") %>%
           str_remove("\\]")) %>% 
  add_count(publisher) %>%
  filter(n > 50) %>%
  left_join(ratings, by = "id") %>% 
  group_by(publisher) %>%
  summarise(med_rating = median(bayes_average), n=max(n)) %>%
  arrange(desc(med_rating))

## Do people like games for 1, 2, 4, 8 players more?

## What are the most popular games per playingtime and age?

count(details, playingtime, sort = T)

top_games <- details %>%
  left_join(ratings, by = "id") %>%
  mutate(playingtime_cut = cut(playingtime, 
                               breaks = c(0, 15, 30, 45, 60, 120, 500000),
                               labels = c("<15", "15-30", "30-45", "45-60", "60-120", "120+"))) %>%
  mutate(minage_cut = cut(minage,
                          breaks = c(0, 6, 8, 10, 12, 16, 100),
                          labels = c("<6", "6-8", "8-10", "10-12", "12-16", "16+"))) %>%
  filter(!is.na(minage_cut), !is.na(playingtime_cut)) %>%
  group_by(playingtime_cut, minage_cut) %>%
  top_n(1, wishing + wanting + owned) %>%
  select(playingtime_cut, minage_cut, bayes_average, owned, wanting, wishing, primary, thumbnail) %>%
  mutate(textlen = ifelse(nchar(primary) > 25, 3, 4))

#try with ggimage and geom_image
top_games %>%
  ggplot(aes(x = playingtime_cut, y = minage_cut)) +
  geom_image(aes(image = thumbnail), asp = 1.3) +
  geom_label(aes(label = str_wrap(primary,28), size = textlen), nudge_y = -0.4) +
  #geom_point(aes(y = as.numeric(minage_cut), size = wishing + wanting + owned)) +
  scale_size_identity()


## top two-player games

twoplayers <- details %>%
  filter(minplayers == 2, maxplayers == 2) %>%
  left_join(ratings, by = "id")
         