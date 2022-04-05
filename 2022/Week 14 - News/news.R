library(ggplot2)
library(dplyr)
library(tidytext)
library(ggstream)
library(forcats)

news_orgs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

sysfonts::font_add_google(name = "Abel", "Abel")
sysfonts::font_add_google(name = "Fredoka One", "Fredoka")
showtext::showtext_auto()

##

topics <- news_orgs %>%
  filter(year_founded >= 2010) %>%
  count(year_founded, is_owner_founder)

topics %>%
  ggplot() +
  geom_stream(aes(x = year_founded, y = n, fill = is_owner_founder),
              type = "proportional") +
  labs(title = "Is the owner the founder?",
       subtitle = "Percentage of recently founded digitally focused local news organizations in the US and Canada",
       caption = "n = 741 | Data: Project Oasis by way of Data is Plural",
       x = "Year founded") +
  annotate("text", x = 2013, y = 0.6, label = "No", size = 12, 
           col = "grey40", family = "Fredoka") +
  annotate("text", x = 2019, y = 0.15, label = "Yes", size = 12, 
           col = "gold3", family = "Fredoka") +
  scale_fill_manual(values = c("#929292", "#F3DF4D")) +
  theme_light() +
  theme(legend.position = "none",
        text = element_text(family = "Abel", size = 14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10))

  
ggsave("2022/Week 14 - News/plot.png", width = 9, height = 6)

## Other ideas


variable <- "revenue_streams"
#variable <- ""

topics <- news_orgs %>%
  filter(year_founded >= 2000) %>%
  filter(!is.na(eval(parse(text = variable)))) %>%
  unnest_tokens(word, eval(parse(text = variable)), token = stringr::str_split, pattern = ", ") %>%
  count(year_founded, word) %>%
  mutate(lump = fct_lump_min(word, 10)) %>%
  count(year_founded, lump, wt = n) %>%
  add_count(lump, sort = T) %>%
  mutate(lump = as.character(lump))

topics %>%
  ggplot() +
  geom_stream(aes(x = year_founded, y = n, fill = lump))
