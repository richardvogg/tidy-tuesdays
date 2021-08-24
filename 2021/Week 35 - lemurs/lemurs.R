library(ggplot2)
library(dplyr)
library(ggraph)
library(igraph)
library(stringr)
library(ggrepel)

lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

## Exploration
#most frequent taxons
lemurs %>% count(dlc_id, taxon) %>% 
  filter(n > 50) %>%
  count(taxon, sort = TRUE)

#LCAT =	Ring-tailed lemur
#CMED = Fat-tailed dwarf lemur

ids <- lemurs %>% filter(taxon == "CMED") %>%
  count(dlc_id, sort = TRUE) %>%
  filter(n > 100) %>% .$dlc_id

#most frequent measured


## Check dlc_id = 3621, Nighthawk

lemurs %>% filter(dlc_id %in% ids) %>%
  mutate(winter_weight = ifelse(month_of_weight %in% 6:8, "Y", "N")) %>%
  ggplot(aes(x = weight_date, y = weight_g, group = dlc_id, 
             col = winter_weight)) + 
  geom_line(size = 1) +
  facet_wrap(~ dlc_id)


## Network with ggraph
#check for how many individuals we know father and mother

set.seed(1)
OGG_tree <- lemurs %>%
  distinct(taxon, dlc_id, lemur_name = name, dam_id, dam_dob, sire_id, sire_dob,
           sex, dob) %>% 
  mutate(dam_id = ifelse(str_detect(dam_id, "[:alpha:]"), NA, dam_id),
         sire_id = ifelse(str_detect(sire_id, "[:alpha:]"), NA, sire_id)) %>%
  filter(taxon == "OGG")


edges <- OGG_tree %>%
  distinct(from = dam_id, to = dlc_id) %>%
  rbind(OGG_tree %>% distinct(from = sire_id, to = dlc_id))

vertices <- OGG_tree %>%
  distinct(name = dlc_id, lemur_name, dob) %>%
  rbind(OGG_tree %>% distinct(name = sire_id, lemur_name = NA, dob = sire_dob)) %>%
  rbind(OGG_tree %>% distinct(name = dam_id, lemur_name = NA, dob = dam_dob)) %>%
  group_by(name) %>%
  summarise(lemur_name = max(lemur_name, na.rm = TRUE),
            dob = max(dob, na.rm = TRUE))

g <- graph_from_data_frame(edges, vertices = vertices)

ggraph(g, layout = "circle") + 
  geom_node_text(aes(label = lemur_name), 
                 size =3) +
  geom_edge_link(alpha = 0.5) 



## try with a manual network

set.seed(8)
lemurs_ogg <- OGG_tree %>% 
  select(lemur_name, dob, dlc_id, dam_id, sire_id, sex) %>%
  mutate(jitter = runif(125))

lemurs_final <- lemurs_ogg %>%
  left_join(lemurs_ogg, by = c("dam_id" = "dlc_id"), suffix = c("",".mom")) %>%
  left_join(lemurs_ogg, by = c("sire_id" = "dlc_id"), suffix = c("", ".dad"))

ggplot(lemurs_final, aes(y = dob, x = jitter)) + 
  geom_text_repel(aes(label = lemur_name, col = sex)) +
  geom_segment(aes(yend = dob.mom, xend = jitter.mom), alpha = 0.2) +
  geom_segment(aes(yend = dob.dad, xend = jitter.dad), alpha = 0.2) +
  labs(title = "Family tree of Northern greater galago lemurs",
       caption = "Data: Duke Lemur Center",
       y = "Date of birth") +
  theme_light() +
  theme(text = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 25),
        plot.caption = element_text(size = 12),
        panel.grid = element_blank())


## try with a different taxon

PCOQ_tree <- lemurs %>%
  distinct(taxon, dlc_id, lemur_name = name, dam_id, dam_dob, sire_id, sire_dob,
           sex, dob) %>% 
  mutate(dam_id = ifelse(str_detect(dam_id, "[:alpha:]"), NA, dam_id),
         sire_id = ifelse(str_detect(sire_id, "[:alpha:]"), NA, sire_id)) %>%
  filter(taxon == "PCOQ")

set.seed(8)


lemurs_list <- PCOQ_tree %>% 
  select(lemur_name, dob, dlc_id, dam_id, sire_id, sex) %>%
  mutate(jitter = runif(133))

lemurs_final <- lemurs_list %>%
  left_join(lemurs_list, by = c("dam_id" = "dlc_id"), suffix = c("",".mom")) %>%
  left_join(lemurs_list, by = c("sire_id" = "dlc_id"), suffix = c("", ".dad"))

ggplot(lemurs_final, aes(y = dob, x = jitter)) + 
  geom_text_repel(aes(label = lemur_name, col = sex)) +
  geom_segment(aes(yend = dob.mom, xend = jitter.mom), alpha = 0.2) +
  geom_segment(aes(yend = dob.dad, xend = jitter.dad), alpha = 0.2) +
  labs(title = "Family tree of Coquerel’s sifaka lemurs",
       caption = "Data: Duke Lemur Center",
       y = "Date of birth") +
  theme_light() +
  theme(text = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 25),
        plot.caption = element_text(size = 12),
        panel.grid = element_blank())
