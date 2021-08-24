library(ggplot2)
library(dplyr)
library(igraph)
library(ggraph)

tuesdata <- tidytuesdayR::tt_load(2021, week = 33)

ipd <- tuesdata$ipd
chain <- tuesdata$chain_investment
investment <- tuesdata$investment

investment %>%
  filter(meta_cat == "Social") %>%
  ggplot(aes(x = year, y = gross_inv, col = category, group = category)) +
  geom_line()

#make a network

edges <- investment %>%
  distinct(from = meta_cat, to = category)

inv_2017 <- investment %>%
  filter(year == 2017)

vertices <- inv_2017 %>%
  distinct(name = meta_cat, val = 0, parent = NA) %>%
  rbind(inv_2017 %>% 
          select(name = category, val = gross_inv, parent = meta_cat)) %>%
  distinct(name, val, parent)
  

g <- graph_from_data_frame(edges, vertices = vertices)

ggraph(g, layout = "fr") + geom_edge_link() +
  geom_node_text(aes(label = stringr::str_wrap(name,30)))

