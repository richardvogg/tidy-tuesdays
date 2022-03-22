library(ggplot2)
library(dplyr)

sysfonts::font_add_google(name = "Fredoka", "Fredoka")
showtext::showtext_auto()

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')


# read president names and terms
# president file from https://gist.github.com/namuol/2657233

presidents <- read.csv("2022/Week 12 - Babynames/presidents.csv") %>%
  mutate(first_name = President %>% 
           strsplit(" ") %>%
           lapply(function(x) x <- x[1]) %>%
           unlist(),
         start_year = Took.office %>%
           strsplit("/") %>%
           lapply(function(x) x <- x[3]) %>%
           unlist() %>%
           as.numeric(),
         end_year = Left.office %>%
           strsplit("/") %>%
           lapply(function(x) x <- x[3]) %>%
           unlist() %>%
           as.numeric()
         ) %>%
  select(Presidency, President, first_name, start_year, end_year, Party) %>%
  mutate(Party = trimws(Party)) %>%
  filter(end_year > 1881)

joined_df <- presidents %>%
  left_join(babynames, by = c("first_name"="name")) %>%
  filter(sex == "M", (year > start_year - 4) & (year < end_year + 4) )


joined_df %>%
  ggplot(aes(x = as.integer(year), y = n, group = President, col = Party)) + 
  geom_line(size = 1) +
  geom_vline(aes(xintercept = start_year)) +
  geom_vline(aes(xintercept = end_year)) +
  geom_label(data = . %>% group_by(President) %>% filter(n == max(n)), 
             aes(label = n), size = 3, show.legend = FALSE) +
  geom_label(data = . %>% group_by(President) %>% filter(n == min(n)), 
            aes(label = n), size = 3, show.legend = FALSE) +
  scale_x_continuous(breaks= scales::pretty_breaks(), expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  scale_color_manual(values = c("blue", "red")) +
  facet_wrap(~reorder(President, Presidency), scales = "free") +
  labs(title = "Newborn boys named after the current president in the U.S.",
       caption = "Data: {babynames} R package") +
  theme_light() +
  theme(axis.title = element_blank(),
        legend.position = c(0.7, 0.1),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(colour = "black"),
        text = element_text(family = "Fredoka", size = 12),
        plot.title = element_text(size = 25),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
  
ggsave("2022/Week 12 - Babynames/plot.png", width = 11, height = 8)
