library(ggHoriPlot)
library(dplyr)
library(stringr)
library(tidyr) #expand function to create all combinations of two variables


tuesdata <- tidytuesdayR::tt_load(2021, week = 38)
billboard <- tuesdata$billboard
rm(tuesdata)

#Convert date into Date format
billboard <- billboard %>%
  mutate(date = as.Date(week_id, "%m/%d/%Y"))



#Find top 15 performer
top_n_performer <- billboard %>%
  count(performer, sort = TRUE) %>%
  slice(1:15) %>% .$performer

#Get combinations of all weeks for all performers (to account for weeks where
#the performer was not on the billboard)
all_weeks <- billboard %>%
  distinct(date) %>%
  mutate(performer = c(top_n_performer, rep("The Beatles", 3264))) %>%
  expand(date, performer)

#Get data for the top 15 performer
top_bands <- billboard %>%
  filter(performer %in% top_n_performer) %>%
  full_join(all_weeks, by = c("date", "performer")) %>%
  group_by(date, performer) %>%
  summarise(best_position = min(week_position, na.rm = TRUE)) %>%
  group_by(performer) %>%
  mutate(min_peak = min(date[is.finite(best_position)]),
         best_position = ifelse(is.infinite(best_position), NA, best_position))

#Visual
top_bands %>% 
  ggplot() +
  geom_horizon(aes(x = date, y = best_position),  origin = 50, horizonscale = c(1,2,10,50,100)) +
  scale_fill_manual(values = c("lightblue", "blue", "darkorange", "red"), label = c("Top100", "Top 50", "Top 10", "Top 1")) +
  facet_wrap(~reorder(performer, min_peak), ncol = 1, strip.position = 'left') +
  scale_x_date(expand=c(0,0), date_breaks = "10 years", date_labels = "%Y") +
  labs(title = "Top 15 performer with most days on the US Billboard",
       caption = "Data: Data.World by way of Sean Miller, Billboard.com and Spotify",
       x = "", fill = "") +
  theme_bw() +
  theme(panel.spacing.y=unit(0.1, "lines"), #no spaces between songs
        strip.text.y.left = element_text(angle = 0, size = 15),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 25),
        legend.position = c(0.9, 0.9),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))