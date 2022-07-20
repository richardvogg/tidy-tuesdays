library(ggplot2)
library(dplyr)
library(tidyr)
library(MetBrewer)
library(stringr)

poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')


category <- "GROWTH"

wordstoremove <- c("Company", "The ", " Corporation", "Association", "Insurance")

df <- reputation %>%
  mutate(company = str_remove_all(company, 
                                  regex(str_c("\\b",wordstoremove, "\\b", 
                                              collapse = '|')))) %>%
  group_by(company) %>%
  mutate(internal_max = rank(-score)==1) %>%
  ungroup() %>%
  group_by(industry, name, internal_max) %>%
  summarise(n = sum(internal_max), companies = list(company)) %>%
  group_by(industry, name) %>%
  mutate(rank = rank(-n)) %>% 
  filter(rank == 1) %>%
  ungroup() %>%
  add_count(industry, wt = n) %>% 
  add_count(name, wt = n) %>%
  mutate(companies = lapply(companies, function(x) paste( unlist(x), collapse=' / ')) %>%
           unlist())

df %>%
  ggplot(aes(x = reorder(name, -nnn), y = reorder(industry,nn), fill = n)) +
  geom_tile() +
  geom_text(data = filter(df, n > 0), aes(label = stringr::str_wrap(companies, 24), 
                                          col = 6L * (n < 4)), 
            size = 3, lineheight = .7) +
  scale_fill_gradientn(colors = met.brewer("OKeeffe2")) +
  scale_color_gradientn(colors = met.brewer("OKeeffe2")) +
  labs(x = "Most valued category", y = "Industry") +
  theme(legend.position = "none")
  
  
  
reputation %>%
  ggplot(aes(x= name, y = score, group = company)) +
  geom_point(size = 1) +
  geom_line(alpha = 0.2, col = "grey") +
  scale_color_manual(values = c("grey")) +
  theme(legend.position = "none")


data.frame()

