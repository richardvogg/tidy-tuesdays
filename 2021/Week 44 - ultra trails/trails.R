library(ggplot2)
library(dplyr)
library(ggsankey)

ultra_rankings <- read.csv("https://raw.githubusercontent.com/BjnNowak/UltraTrailRunning/main/ranking.csv")
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

## Idea - show who travels where to race

race_small <- race %>%
  select(race_year_id, country) %>%
  mutate(country = strsplit(country, ", ") %>%
           lapply(function(x) x <- ifelse(length(x) == 1 | x[1] == "Hong Kong", x[1], x[2])) %>% 
           unlist())

nationalities <- ultra_rankings %>%
  left_join(race_small, by = c("RaceYearId" = "race_year_id")) %>% 
  count(Nationality, country, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(Nationality = countrycode::countrycode(Nationality, 
                                                origin = "ioc", 
                                                destination = "country.name")) %>%
  filter(Nationality != country)


edges <- nationalities %>%
  rename(`runners from` = Nationality, `travel to run in` = country)

sankey_df <- edges %>%
  make_long(`runners from`, `travel to run in`, value = n) %>%
  mutate(in_france = ifelse(next_node == "France", 1, 0),
         in_france = ifelse(is.na(next_x), NA, in_france),
         node_name = ifelse(value > 100, node, "."))

ggplot(sankey_df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               value = value)) +
  geom_sankey(alpha = 0.5) +
  geom_sankey_label(aes(label = node), size = 3, color = "white", fill = "gray40") +
  labs(title = "Ultra trails in France attract people from all over the world",
       caption = "Data: Benjamin Nowak by way of International Trail Running Association") +
  scale_x_discrete(position = "top") +
  theme_sankey() +
  theme(legend.position = "none",
        axis.title.x.top = element_blank(),
        axis.text.x.top = element_text(size = 16),
        plot.title = element_text(size = 20))



## ggraph try out

library(ggraph)
library(igraph)

travel <- igraph::graph_from_data_frame(edges)

ggraph(travel) + geom_edge_arc(arrow = arrow(length = unit(4, 'mm')), 
                               start_cap = circle(3, 'mm'),
                               end_cap = circle(3, 'mm'), alpha = 0.2) +
  geom_node_text(aes(label = name))

