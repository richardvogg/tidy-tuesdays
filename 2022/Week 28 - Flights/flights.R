library(dplyr)
library(ggplot2)
library(ggradar)
library(lubridate)

flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

sysfonts::font_add_google(name = "Abel", "Abel")
showtext::showtext_auto()

total_df <- flights %>%
  filter(!STATE_NAME %in% c("Morocco", "Israel")) %>%
  mutate(date = as_date(paste0(YEAR,MONTH_NUM,"01")),
         STATE_NAME = ifelse(STATE_NAME == "Republic of North Macedonia",
                             "North Macedonia", STATE_NAME)) %>%
  group_by(month_year = floor_date(date, unit = "month"), country = STATE_NAME) %>%
  summarise(flights = sum(FLT_TOT_1, na.rm = TRUE)) 

total_df %>%
ggplot(aes(x = month_year, y = flights)) +
  geom_line(col = "#458ECD") +
  geom_text(data = subset(total_df, month_year == "2019-04-01"), 
            aes(label = flights, 
                x = as_date("2019-02-01"),
                y = flights * 0.7), 
            size = 3, family = "Abel") +
  geom_text(data = subset(total_df, month_year == "2020-04-01"), 
            aes(label = flights, 
                x = as_date("2019-07-01"), 
                y = ifelse(flights > 22000, flights * 1.2, flights * 2.2)),
            size = 3, family = "Abel") +
  facet_wrap(~country, ncol = 5, scales = "free_y") +
  labs(title = "Commercial flights in Europe and their drop during the pandemic",
       subtitle = "Number of flights in April 2019 vs April 2020 displayed",
       caption = "Data: Eurocontrol") +
  theme_light() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "#458ECD"),
        strip.text = element_text(family = "Abel", size = 12, colour = "#458ECD"),
        plot.title = element_text(family = "Abel", size = 25),
        plot.subtitle = element_text(family = "Abel", size = 20),
        plot.caption = element_text(family = "Abel", size = 12))

ggsave("2022/Week 28 - Flights/plot.png", width = 9, height = 10)
  