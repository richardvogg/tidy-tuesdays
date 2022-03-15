library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(ggchicklet)
library(geomtextpath)
library(patchwork)

Sys.setlocale("LC_TIME","en_US")

sysfonts::font_add_google(name = "Fredoka", "Fredoka")
showtext::showtext_auto()

bioc <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/bioc.csv')
cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')


test <- cran %>%
  mutate(date = str_replace(date, "  ", " ")) %>%
  mutate(datebla = strsplit(date, " ") %>% 
           lapply(function(x) length(x)) %>% 
           unlist()) %>%
  mutate(date_clean = case_when(
    datebla == 3 ~ ymd_hms(date),
    #move year to the beginning so that ymd_hms can do its magic
    datebla == 5 ~ ymd_hms(str_c(substr(date, nchar(date) - 3, nchar(date)),
                                         substr(date, 1, nchar(date) - 4)))
  ))

table(test$datebla)
#We see that there are three types of dates
#missing (0)
#older dates in format Fri Jun 24 11:43:01 2005
#new dates in format 2019-07-28 14:50:25 UTC


overall <- test %>%
  filter(date_clean > as.Date("2000-01-01"), date_clean < as.Date("2021-01-01")) %>%
  mutate(date_floor = floor_date(date_clean,'month')) %>%
  count(date_floor) %>%
ggplot(aes(x = date_floor, y = n, group = 1)) + 
  geom_line(col = "darkorange") +
  labs(subtitle = "by year and month") +
  theme_light() +
  theme(axis.title = element_blank())


seasonal <- test %>%
  filter(date_clean >= as.Date("2000-01-01"), date_clean < as.Date("2021-01-01")) %>%
  mutate(month = month(date_clean)) %>%
  count(month) %>%
  ggplot(aes(x = reorder(month.abb[month], month), y = n)) + 
  geom_chicklet(fill = "darkorange", alpha = 0.6) +
  labs(subtitle = "by month") +
  theme_light() +
  theme(axis.title = element_blank())


weekdays <- test %>%
  filter(date_clean > as.Date("2000-01-01"), date_clean < as.Date("2021-01-01")) %>%
  mutate(day = factor(weekdays(date_clean), levels=c("Monday","Tuesday", "Wednesday", 
                                                     "Thursday", "Friday", "Saturday","Sunday"))) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) + 
  geom_chicklet(fill = "darkorange", alpha = 0.6) +
  labs(subtitle = "by weekday") +
  theme_light() +
  theme(axis.title = element_blank())


hours <- test %>%
  filter(date_clean > as.Date("2000-01-01"), date_clean < as.Date("2021-01-01")) %>%
  mutate(hour = hour(date_clean)) %>%
  count(hour)
  

am <- hours %>%
  filter(hour < 12) %>%
  mutate(hour = ifelse(hour == 0, 12, hour)) %>% 
  ggplot(aes(x = hour, y = n)) + 
  geom_segment(aes(yend = 500, xend = hour), size = 2, alpha = 0.6, col = "darkorange") +
  scale_x_continuous(limits = c(0, 12), breaks = 1:12,
                     label = as.roman) +
  scale_y_continuous(expand = c(0,0), limits = c(0, max(hours$n))) +
  coord_curvedpolar() +
  labs(subtitle = "AM") +
  theme_void() + 
  theme(axis.text.x = element_text(size = 20),
        plot.margin = margin(10, 30, 10, 30))

pm <- hours %>%
  filter(hour >= 12) %>%
  mutate(hour = ifelse(hour > 12, hour - 12, hour)) %>%
  ggplot(aes(x = hour, y = n)) + 
  geom_segment(aes(yend = 500, xend = hour), size = 2, alpha = 0.6, col = "darkorange") +
  scale_x_continuous(limits = c(0, 12), breaks = 1:12,
                     label = as.roman) +
  scale_y_continuous(expand = c(0,0), limits = c(0, max(hours$n))) +
  coord_curvedpolar() +
  labs(subtitle = "PM") +
  theme_void() + 
  theme(axis.text.x = element_text(size = 20),
        plot.margin = margin(10, 30, 10, 30))


(overall + seasonal + plot_layout(widths = c(2,2))) / 
  (weekdays + am + pm + plot_layout(widths = c(2,1,1))) +
  plot_annotation(title = "R Package version releases on CRAN",
                  caption = "Data: Robert Flight") &
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 16),
        text = element_text(family = "Fredoka"))

  
