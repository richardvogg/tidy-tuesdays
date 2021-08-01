library(ggplot2)
library(dplyr)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2021, week = 31)

olympics <- tuesdata$olympics
rm(tuesdata)

top_25_sports <- olympics %>%
  filter(season == "Summer") %>%
  count(sport, sort = TRUE) %>% 
  slice(1:25) %>%
  .$sport

olympics_count <- olympics %>%
  filter(season == "Summer") %>%
  filter(sport %in% top_25_sports) %>%
  count(year, sex, sport) %>%
  add_count(year, sport, wt = n, sort = TRUE) %>%
  mutate(perc = n / nn)

olympics_count %>%
  ggplot(aes(x = year, y = perc, fill = sex), col = "#77887e", width = 2) + 
  geom_col() +
  geom_hline(yintercept = 0.5, color = "#77887e", size = 1) +
  scale_fill_manual(values = c("#f3be0c", "#0C41F3")) +
  facet_wrap(~sport, scales = "free_y") +
  theme_light() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 15),
        axis.text.x = element_text(size = 12))


top_25_country <- olympics %>%
  filter(season == "Summer") %>%
  count(team, sort = TRUE) %>% 
  slice(1:25) %>%
  .$team

olympics_count <- olympics %>%
  filter(season == "Summer") %>%
  filter(team %in% top_25_country) %>%
  count(year, sex, team) %>%
  add_count(year, team, wt = n, sort = TRUE) %>%
  mutate(perc = n / nn)

olympics_count %>%
  ggplot(aes(x = year, y = perc, fill = sex), col = "#77887e", width = 2) + 
  geom_col() +
  geom_hline(yintercept = 0.5, color = "#77887e", size = 1) +
  scale_fill_manual(values = c("#f3be0c", "#0C41F3")) +
  facet_wrap(~team, scales = "free_y") +
  theme_light() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 15),
        axis.text.x = element_text(size = 12))






#tallest and smallest

smallest <- olympics %>%
  filter(year == 2016) %>%
  group_by(sex) %>%
  arrange(height) %>%
  slice_head(prop = 0.01)

tallest <- olympics %>%
  filter(year == 2016) %>%
  group_by(sex) %>%
  arrange(desc(height)) %>%
  slice_head(prop = 0.01)


tallest %>%
  count(sport, sex) %>%
  ggplot(aes(x = n, y = sport)) + 
  geom_col() +
  facet_wrap(~sex)


### Size and weight

olympics %>%
  filter(season == "Summer", year %in% c(1988, 2016)) %>%
  group_by(year, sport, sex) %>%
  summarise(avg_weight = mean(weight, na.rm = TRUE), 
            avg_height = mean(height, na.rm = TRUE)) %>%
  filter(!is.nan(avg_weight) & !is.nan(avg_height)) %>% 
  ggplot(aes(x = avg_weight, y = avg_height)) +
  geom_point(size = 0.2) +
  geom_text(aes(label = paste0(year, sport, sex)))
