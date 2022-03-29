library(ggplot2)
library(dplyr)
library(tidyr)
library(scatterpie)
library(ggrepel)
library(ggtext)

options(scipen = 10)

sysfonts::font_add_google(name = "Fredoka", "Fredoka")
sysfonts::font_add_google(name = "Sriracha", "Sriracha")
showtext::showtext_auto(enable = TRUE)

sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

summary <- sports %>%
  select(year, sports, exp_women, exp_men, total_exp_menwomen) %>%
  group_by(year, sports) %>%
  summarise(exp_women = sum(exp_women, na.rm = TRUE), 
            exp_men = sum(exp_men, na.rm = TRUE),
            exp_total = sum(total_exp_menwomen, na.rm = TRUE)) %>%
  filter(year %in% c(2015, 2019), exp_total > 0) %>%
  mutate(perc_women = 5 * (exp_women / exp_total),
         exp_total = exp_total / 1000000) %>%
  ungroup()
  

change <- summary %>%
  pivot_wider(id_cols = c(sports), 
              names_from = year, values_from = c(exp_total, perc_women))

ggplot(summary %>% filter(year == 2019)) +
  geom_smooth(aes(x = perc_women, y = exp_total), method = "lm", alpha = 0.2, col = "white", size = 0.5) +
  geom_segment(data = change, aes(y = exp_total_2015, yend = exp_total_2019,
                                  x = perc_women_2015, xend = perc_women_2019)) +
  geom_point(data = change, aes(y = exp_total_2015, x = perc_women_2015), 
             size = 2, color = "white") +
  geom_scatterpie(aes(x = perc_women, y = exp_total, group = factor(sports)), 
                  data = summary %>% filter(year == 2019), 
                  cols = c("exp_women", "exp_men"),
                  pie_scale = 0.7, color = "white") +
  geom_text_repel(aes(x = perc_women, y = exp_total, label = sports), 
                  size = 3, family = "Fredoka") +
  
  
  coord_fixed() +
  scale_fill_manual(values = c("orange", "blue")) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1, 4), 
                     labels = c("more $ for men", "more $ for women")) +
  labs(title = "Expenditures for <span style='color:blue'>men</span> and <span style='color:orange'>women</span> in Collegiate Sports in the US 2019",
       subtitle = "White dots show values from 2015",
       caption = "Data: Equity in Athletics Data Analysis",
       x = "",
       y = "Total expenditure in Million US-$") +
  theme(
    legend.position = "none",
    
    plot.subtitle = element_text(size = 15),
    panel.background = element_rect(fill = "grey70"),
    axis.text = element_text(size = 12),
    text = element_text(size = 12),
    plot.title = element_markdown(size = 20, hjust = 0)
  )
  
ggsave("2022/Week 13 - Sports/plot.png", width = 11, height = 8)

