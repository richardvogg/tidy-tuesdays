library(dplyr)
library(ggplot2)
library(tidytext)
library(tidylo) #log odds
library(forcats)

nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

sysfonts::font_add_google(name = "Abel", "Abel")
showtext::showtext_auto()


## Log odds per month

nyt_titles %>%
  unnest_tokens(words, title) %>% 
  mutate(month_num = as.numeric(format(first_week, "%m")),
         month = month.abb[month_num]) %>%
  count(month, month_num, words) %>%
  anti_join(stop_words,by = c("words"="word")) %>%
  add_count(words, wt = n) %>%
  filter(nn>16) %>%
  bind_log_odds(month,words,n) %>%
  filter(log_odds_weighted > 2) %>%
  mutate(words_fill = case_when(
    words == "christmas" ~ "christmas",
    words == "beach" ~ "beach",
    TRUE ~ "other"
  )) %>%
  
  ggplot(aes(x= reorder_within(words, log_odds_weighted, month), log_odds_weighted)) +
  geom_col(aes(fill = words_fill), show.legend = FALSE, width = 0.8, size = 1, 
           col = "grey50") +
  geom_text(aes(label = words), y = 0.1, hjust = 0, size = 4, family = "Abel") +
  labs(x = NULL, y = "log odds") +
  scale_fill_manual(values = c("yellow2", "orange", NA)) +
  facet_grid(reorder(month, month_num)~., space = "free", scales = "free_y",
             switch = "y") +
  labs(title = "Christmas and the beach inspire",
       subtitle = "Which words in NYT bestseller titles appear disproportionally often in which month?",
       caption = "Data: Post 45") +
  coord_flip() +
  scale_x_reordered() +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size = 14, family = "Abel"),
        plot.title = element_text(size = 20))

ggsave("2022/Week 19 - NYT/plot.png", width = 8, height = 10)

### Other ideas




top_words <- nyt_titles %>%
  unnest_tokens(words, title) %>%
  mutate(lettercount = nchar(words)) %>%
  filter(lettercount > 1) %>%
  mutate(lettercount = ifelse(lettercount >= 8, "8+", lettercount)) %>%
  count(words, lettercount, sort = TRUE) %>%
  group_by(lettercount) %>%
  mutate(rank = rank(n, ties.method = "random")) %>%
  top_n(6, rank)

top_words %>%
  ggplot(aes(x = 1, y = reorder(words, n))) +
  geom_col() +
  geom_text(aes(label = words, x = 0.5), col = "white") +
  facet_wrap(~lettercount, nrow = 1, scales = "free_y") +
  labs(title = "Make your own bestseller title") +
  theme_light() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


summer_words <- c("summer", "hot", "beach")
winter_words <- c("christmas", "winter", "cold")

nyt_titles %>%
  unnest_tokens(words, title) %>%
  filter(words %in% c(summer_words, winter_words)) %>%
  mutate(season = ifelse(words %in% summer_words, "summer", "winter")) %>%
  mutate(month_num = as.numeric(format(first_week, "%m")),
    month = month.abb[month_num]) %>%
  count(month, month_num, season) %>%
  ggplot(aes(x = reorder(month, month_num), y = n)) +
  geom_col() +
  facet_wrap(~season)


