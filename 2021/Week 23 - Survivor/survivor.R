library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtext)

#fonts
sysfonts::font_add_google(name = "Roboto","Roboto")
showtext::showtext_auto()


castaways <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/castaways.csv')

df <- castaways %>%
  select(season, personality_type, order) %>%
  filter(!is.na(personality_type)) %>%
  separate(personality_type, into = c("mind", "energy", "nature", "tactics"), 
           sep = 1:3) %>%
  group_by(season) %>%
  mutate(order_rev = pmin(18, max(order) - order + 1)) %>%
  ungroup()
  
df %>%
  count(order_rev, mind) %>%
  ggplot(aes(x = order_rev, y = n, fill = mind, col = mind, alpha = order_rev %in% c(1:3, 15:18))) + 
  geom_col(position = "dodge") +
  #geom_stream(alpha = 0.5, type = "proportional") +
  labs(title = "Survivor participants and extraversion",
  subtitle = glue::glue("<span style='color:darkorange;'>Extroverts</span> are often voted out early - but more likely to make it to the final 3
                          compared to <span style='color:darkblue;'>introverts</span>"),
       x = "Final position") +
  scale_x_continuous(breaks = 1:18, labels = c(1:17, ">18")) +
  scale_fill_manual(values = c("darkorange","darkblue")) +
  scale_color_manual(values = c("darkorange", "darkblue")) +
  scale_alpha_discrete() +
  theme_light() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_markdown(size = 16),
        plot.title = element_text(size = 20),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.position = "none",
        text = element_text(family = "Roboto"))


