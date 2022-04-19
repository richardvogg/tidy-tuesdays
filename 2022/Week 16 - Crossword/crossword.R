library(ggplot2)
library(dplyr)
library(tidyr)
library(tidytext)
library(janeaustenr)
library(ggtext)

options(scipen = 10)

sysfonts::font_add_google(name = "Roboto", "Roboto")
sysfonts::font_add_google(name = "Damion", "Damion")
showtext::showtext_auto()

big_dave <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv')
times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv')


times_df <- times %>%
  unnest_tokens(letter, answer, token = "characters") %>%
  count(letter) %>%
  filter(!is.na(letter), n > 100) %>%
  mutate(perc = n / sum(n))

bigdave_df <- big_dave %>%
  unnest_tokens(letter, answer, token = "characters") %>%
  count(letter) %>%
  filter(!is.na(letter), n > 100) %>%
  mutate(perc = n / sum(n))


pridepre <- tibble(txt = prideprejudice)

pride_df <- pridepre %>%
  #remove stopwords first maybe?
  unnest_tokens(letter, txt, token = "characters") %>%
  count(letter) %>%
  filter(!is.na(letter), n > 100) %>%
  mutate(perc = n / sum(n))


final_df <- times_df %>%
  inner_join(bigdave_df, by = "letter", suffix = c("_times", "_bigdave")) %>%
  inner_join(pride_df, by = "letter") %>%
  pivot_longer(cols = c("perc_times", "perc_bigdave")) %>%
  mutate(percdiff = (perc - value)/value) %>%
  mutate(percdiff_cut = cut(percdiff, breaks = c(-1, -0.2, 0.2, 2)))
  

ggplot(final_df, aes(x = name, y = value)) +
  geom_col(aes(fill = percdiff_cut)) +
  geom_text(aes(x = 0.1, y = 0.1, label = toupper(letter)), 
            size = 10, color = "grey80", family = "Damion") +
  geom_hline(aes(yintercept = perc)) +
  scale_fill_manual(values = c("blue", "grey", "orange")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = c("Big Dave", "NY Times")) +
  facet_wrap(~letter) +
  expand_limits(x = -0.3) +
  labs(title = "Crossword puzzle words use different letters than novels",
       caption = "Data: Cryptic Crossword Clues",
       subtitles = "Letters which are used <span style='color:blue'>much more</span> or <span style='color:orange'>much less</span> in crossword puzzles compared to <span style='text-decoration:underline'>Pride and Prejudice</span>.",
       y = "Frequency", x = "") +
  theme_light() +
  theme(strip.text = element_blank(),
        plot.title = element_text(size = 25, margin = margin(t = 4, b = 4)),
        plot.subtitle = element_textbox_simple(size = 15, margin = margin(t = 4, b = 8)),
        legend.position = "none",
        text = element_text(family = "Roboto", size = 12))

ggsave("2022/Week 16 - Crossword/plot.png", width = 11, height = 8)
  