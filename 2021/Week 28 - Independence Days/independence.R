library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggfx)

sysfonts::font_add_google(name = "Jura", "Jura")
showtext::showtext_auto()


holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')

Sys.setlocale("LC_TIME", "C")

months <- data.frame(id = 1:365) %>%
  mutate(date = as.Date(id, origin = "1999-12-31")) %>%
  transmute(id, day = as.numeric(format(date, "%d")),
            month = format(date, "%b"))

month_summ <- months %>%
  group_by(month) %>%
  summarise(id = median(id))



holidays_final <- holidays %>%
  left_join(months, by = c("day", "month")) %>%
  mutate(iso3c = countrycode::countryname(country, 
                                          destination = "iso3c"),
         continent = countrycode::countryname(country, 
                                              destination = "continent")) %>%
  filter(!is.na(iso3c), !is.na(month)) %>%
  #group_by(month, day) %>%
  #summarise(id = max(id), continent = max(continent),
  #          n2 = n(), country = paste(iso3c, collapse = ", ")) %>%
  distinct() %>%
  ungroup()


df <- holidays_final %>%
  filter(continent != "Oceania", year > 1700) %>% 
  arrange(id) %>%
  mutate(Diff = id - lag(id),
         Diff2 = id - lag(id, n = 2)) %>%
  mutate(close = ifelse(Diff < 3 & Diff2 > 5, 1, 0),
         close = ifelse(close == 0 & Diff2 < 5, 2, close),
         close = ifelse(is.na(close), 0, close))

df %>%
  ggplot(aes(x = id, y = 1700, col = reorder(month, id))) +
  
  #year rings
  with_outer_glow(
    geom_hline(yintercept = 1700, col = "grey")
  ) +
  with_outer_glow(
    geom_hline(yintercept = 1800, col = "grey")
  ) +
  with_outer_glow(
    geom_hline(yintercept = 1900, col = "grey")
  ) +
  with_outer_glow(
    geom_hline(yintercept = 2000, col = "grey")
  ) +
  
  annotate("text", label = "1700", x = -25, y = 1670, col = "grey", family = "Jura") +
  annotate("text", label = "1800", x = -25, y = 1780, col = "grey", family = "Jura") +
  annotate("text", label = "1900", x = -25, y = 1880, col = "grey", family = "Jura") +
  annotate("text", label = "2000", x = -25, y = 1980, col = "grey", family = "Jura") +
  
  # color segments
  with_outer_glow(
    geom_segment(aes(xend = id, yend = year), size = 1),
    color = "white"
  ) +
  
  #continent label
  geom_text(data = distinct(df %>% select(continent)),
                            aes(label = continent, y = 1600, x = 0),
            col = "white", family = "Jura") +
  
  #month labels
  geom_text(data = month_summ, aes(label = month, y = 2100), size = 5, family = "Jura") +
  
  
  #dimensions of rings
  expand_limits(y = c(1600, 2050), x = c(-25, 390)) +
  
  #make plot circular
  coord_polar(theta = "x") +
  
  labs(title = "Independence days across the globe.",
       subtitle = "Each segment represents one country's date and year of independence.",
       caption = "Data: Wikipedia") +
  facet_wrap(~continent) +
  theme_void() +
  theme(legend.position = "none",
        strip.text = element_blank(),
        text = element_text(family = "Jura", colour = "white"),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 16),
        plot.background = element_rect(fill = "black"))


#ggsave("2021/Week 28 - Independence Days/plot.png", width = 3, height = 3)
