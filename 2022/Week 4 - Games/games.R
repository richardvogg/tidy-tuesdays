#remotes::install_github("r-link/corrmorant")

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(patchwork)
library(ggimage)
library(corrmorant)
library(geomtextpath)
library(ggchicklet)

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

sysfonts::font_add_google(name = "Josefin Sans", "Josefin")
showtext::showtext_auto()

## More collaboration, less competition

coop_df <- details %>%
  mutate(cooperative = ifelse(str_detect(boardgamemechanic, "Cooperative"), 1, 0)) %>%
  count(cooperative, yearpublished) %>%
  filter(yearpublished >= 2000, yearpublished < 2022, !is.na(cooperative)) %>%
  add_count(yearpublished, wt = n) %>% 
  mutate(prop = n / nn)

coop_plot <- coop_df %>%
  ggplot(aes(x = yearpublished, y = prop, fill = factor(cooperative))) +
  geom_area(alpha = 0.6) +
  geom_textline(data = subset(coop_df, cooperative == 1), 
                label = "Cooperative Games", hjust = 0.9, vjust = 1.5,
                text_smoothing = 50, size = 5, family = "Josefin") +
  annotate("text", x = 2010, y = 0.6,
              label = "Non-cooperative Games", size = 5, family = "Josefin") +
  geom_texthline(yintercept = 0.2, label = "20%",  size = 5, family = "Josefin") +
  scale_fill_manual(values = c("grey80", "aquamarine")) +
  labs(subtitle = "Share of cooperative board games\n per year increases since 2006.") +
  theme_light() +
  theme(legend.position = "none",
        text = element_text(size = 15, family = "Josefin"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 15))

# Top 5 cooperative board games
n <- 3

top_n <- details %>%
  left_join(ratings, by = "id") %>%
  mutate(cooperative = ifelse(str_detect(boardgamemechanic, "Cooperative"), 1, 0)) %>%
  filter(yearpublished >= 2000, yearpublished < 2022, cooperative == 1) %>% 
  mutate(minage_cut = cut(minage,
                          breaks = c(0, 8, 12, 100),
                          labels = c("<8", "8-12", "12+"))) %>%
  filter(!is.na(minage_cut)) %>%
  group_by(minage_cut) %>%
  top_n(n, owned) %>%
  mutate(rank = rank(owned))



top_n_plot <- top_n %>%
  ggplot(aes(y = owned, x = reorder(str_wrap(primary, 25), owned))) + 
  geom_chicklet(fill = "aquamarine", alpha = 0.6) +
  geom_text(aes(label = str_wrap(paste0(primary, " (", yearpublished, ")"), 22), y = 0), 
            hjust = 0, nudge_y = 2000, lineheight = 0.8, family = "Josefin") +
  coord_flip() +
  theme_light() +
  facet_wrap(minage_cut~., scales = "free_y",nrow = 3) +
  labs(subtitle = "Top 3 most owned Cooperative Games\n by Players Minimum Age") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(fill = "grey80"),
        strip.text = element_text(colour = "black"),
        text = element_text(size = 15))
  
coop_plot + top_n_plot +
  plot_annotation(title = "More cooperation, less competition",
                  caption = "Data: BoardGameGeek") &
  theme(plot.title = element_text(size = 30),
        text = element_text(family = "Josefin"))

ggsave("2022/Week 4 - Games/plot.png", width = 12, height = 7)

########
#Other ideas


# Which is the best metric to measure how cool a game is?
# Bayes_average better than average
# Using owned, wanted/wished from the details df

bayes <- ratings %>%
  ggplot(aes(x = bayes_average, y = users_rated)) + geom_point()

normal <- ratings %>%
  ggplot(aes(x = average, y = users_rated)) + geom_point()

bayes + normal

## Correlations?

corrdata <- details %>%
  left_join(ratings, by = "id") %>%
  select(average, bayes_average, wanting, wishing, users_rated, owned, trading) %>%
  filter(bayes_average>0, users_rated > 500)


corrdata %>%
  ggcorrm() +
  utri_heatcircle(alpha = 0.5) +
  lotri(geom_point(alpha = 0.1)) +
  utri_corrtext() +
  dia_names(y_pos = 0.15, size = 3, family = "Josefin") +
  dia_density(lower = 0.3, fill = "lightgrey", color = 1) +
  scale_fill_corr(limits = c(0, 1), option = "B") +
  theme(text = element_text(size = 14, family = "Josefin"),
        axis.text = element_blank())

ggsave("2022/Week 4 - Games/corr.png", width = 8, height = 8)

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

  