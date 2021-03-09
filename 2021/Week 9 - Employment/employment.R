library(dplyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(stringr)

tuesdata <- tidytuesdayR::tt_load(2021, week = 9)

employed <- tuesdata$employed
earn <- tuesdata$earn

#First try
earn %>%
  filter(sex=="Both Sexes", race != "All Races", ethnic_origin == "All Origins") %>%
  mutate(yearq = paste0(year,"-",quarter)) %>%
  filter(age %in% c("16 to 24 years","25 to 54 years","55 years and over")) %>%
  ggplot(aes(x = yearq, y = median_weekly_earn, color = age,group=age))+geom_line(size=1)+
  facet_wrap(~race)




employed <- employed %>%
  filter(!is.na(industry)) %>%
  filter(race_gender=="TOTAL") %>%
  filter(year==2020) %>%
  filter(employ_n != 0 & !is.na(employ_n))

edges <- employed %>%
  distinct(from = ".", to = industry) %>%
  rbind(employed %>%
  distinct(from=industry,to=paste0(industry,".",major_occupation))) %>%
  rbind(employed %>% 
          distinct(from=paste0(industry,".",major_occupation),
                   to=paste0(industry,".",major_occupation,".",minor_occupation)))

# Usually we associate another dataset that give information about each node of the dataset:
vertices <- employed %>% 
  mutate(level=4,short_name=minor_occupation) %>%
  distinct(name=paste0(industry,".",major_occupation,".",minor_occupation),
           size=employ_n, level, short_name) %>%
  select(name,size,level,short_name) %>%
  rbind(employed %>% 
          mutate(size=0,level=3,short_name=paste0(major_occupation)) %>%
          distinct(name=paste0(industry,".",major_occupation), size, level,short_name)) %>%
  rbind(employed %>% distinct(name=industry) %>% mutate(size=0,level=2,short_name=name)) %>%
  rbind(data.frame(name=".",size=0,level=1,short_name=".")) %>%
  mutate(new_label = ifelse(level==2,name,NA),
         new_fill = ifelse(level==4,short_name,NA),
         new_col = ifelse(level==3,short_name,NA))

# Then we have to make a 'graph' object using the igraph library:
mygraph <- graph_from_data_frame( edges, vertices=vertices )


levels3list <- unique(vertices$new_fill[vertices$level==3])

levels4list <- unique(vertices$new_fill[vertices$level==4]) %>%
  str_wrap(20)

colors <- c("darkred","darksalmon","blue","aquamarine1","orange",
            "goldenrod","darkgreen","lightgreen","olivedrab1","grey80","grey40")

names(colors) <- levels4list

set.seed(62)

ggraph(mygraph, layout = 'circlepack',weight=size) +
  geom_node_circle(aes(fill = str_wrap(new_fill,20),col=new_col),size=2) +
  geom_node_text(aes(label=str_wrap(new_label,15))) +
  scale_fill_manual(values=colors,breaks=levels4list,na.translate=FALSE) +
  scale_colour_manual(values=c("red","green","grey60","yellow","lightblue"),
                      breaks=levels3list,na.translate=FALSE) +
  labs(fill="Sub-category")+
  guides(fill=guide_legend(ncol = 5))+
  theme_void()+
  theme(legend.position = c(0.5,0.1))


#legend

vertices %>%
  filter(level%in%3:4) %>%
  distinct(short_name) %>%
  left_join(edges,by=c("short_name"=))

data.frame(col=colors,type=names(colors),row.names = NULL) %>%
  mutate(level=ifelse(stringr::str_detect(type,"^Z_"),3,4)) %>%
  ggplot(aes(x=level,y=1,fill=col))+geom_tile()
