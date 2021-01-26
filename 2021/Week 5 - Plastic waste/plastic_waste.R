library(ggplot2)
library(dplyr)
library(ggbump)
library(fuzzymatch)

tuesdata <- tidytuesdayR::tt_load('2021',5)

plastics <- tuesdata$plastics

## Fuzzy deduplication (e.g. Nestlé vs Nestle)

dedupes <- fuzzymatch::fuzzy_dedupes(plastics$parent_company,find_cutoff=TRUE)

plastics$parent_company <- fuzzymatch::fuzzy_dedupes(plastics$parent_company,cutoff_distance = 0.08)



company_rank <- plastics %>%
  filter(parent_company != "Grand Total") %>%
  mutate(parent_company=ifelse(parent_company %in% c("NULL","null","Inconnu"),
                               "Unbranded",parent_company)) %>%
  group_by(year,parent_company) %>%
  summarise(n=sum(grand_total,na.rm=TRUE)) %>%
  group_by(year) %>%
  mutate(grand_total = sum(n),
         percentage = n/grand_total) %>%
  filter(!parent_company %in% c("Assorted","La Doo","Barna",
                                       "Pure Water, Inc.","Unbranded") ) %>%
  mutate(rank=rank(-percentage))
  
  
top_5 <- company_rank %>%
  top_n(5,percentage) %>%
  {unique(.$parent_company)}

top_5_df <- company_rank %>%
  filter(parent_company %in% top_5) %>% 
  mutate(rank=ifelse(rank>5,6,rank))

top_5_df %>%
  ggplot(aes(x=year,y=rank,color=parent_company)) +
  geom_point(size = 7) +
  geom_text(data = top_5_df %>% filter(year == 2019),
            aes(x = year - .1, label = paste0(parent_company,"\n",100*round(percentage,3),"%")), size = 5, hjust = 1) +
  geom_text(data = top_5_df %>% filter(year == 2020),
            aes(x = year + .1, label = paste0(parent_company,"\n",100*round(percentage,3),"%")), size = 5, hjust = 0) +
  geom_bump(size = 2, smooth = 8) +
  geom_hline(yintercept = 5.5,size=1)+
  scale_x_continuous(limits = c(2018.3, 2020.8),
                     breaks = seq(2019, 2020, 1),
                     position = "top") +
  labs(y = NULL,
       x = NULL,
       title = "Top 5 plastic polluters",
       subtitle = paste0("Volunteers collected 775,000 plastic items in 70 countries worldwide. \n",
                        "The main polluters are large corporations whose plastic can be found all over our planet.\n",
                        "Below the company name we see the share of plastic pieces for each company."),
       caption = "Data: www.breakfreefromplastic.org") +
  scale_y_reverse(breaks=1:5)+
  coord_cartesian(ylim = c(6.2,0.8))+
  theme_light()+
  theme(panel.grid = element_blank(),
    axis.text = element_text(size=15),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=16),
        legend.position = "none")

plastics %>%
  group_by(year,country,parent_company) %>%
  summarise(grand_total=sum(grand_total),volunteers=max(volunteers),events=max(num_events)) %>%
  top_n(3,grand_total) 

plastics %>%
  group_by(year,country) %>%
  summarise(events=max(num_events),volunteers=max(volunteers)) %>%
  count(year,wt=volunteers)

