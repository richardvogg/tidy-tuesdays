library(ggplot2)
library(dplyr)
library(stringr)
library(stringdist)

tuesdata <- tidytuesdayR::tt_load(2020, week = 35)

chopped <- tuesdata$chopped


#Judges
#Remove fuzzy duplicates

#Make judges columns tidy - for our analysis we will ignore order of judges
judge_df <- chopped %>% select(season:judge3) %>% 
  tidyr::pivot_longer(cols=starts_with("judge"),names_to="judge_nr",values_to="judge")

  
#How often did each judge appear?
rank_before <- judge_df %>% 
  dplyr::count(judge,sort=T) %>% 
  tibble() %>% 
  mutate(judge=ifelse(is.na(judge),"None",judge))

names <- rank_before$judge

#The core of fuzzy duplicates - Comparing each judge name with all the judge names before
#We store the closest name and the distance between both
out <- sapply(seq_along(names)[-1],function(i) {
  dist2 <- stringdist(names[i],names[1:i-1],method='jw',p=0.1)
  best_fit <- which.min(dist2)
  similarity2 <- min(dist2)
  return(c(similarity2,best_fit))
}) %>% 
  t() %>%
  as.data.frame() %>% 
  add_row(V1=1,V2=1,.before = 1) %>% 
  cbind(rank_before) %>% 
  dplyr::rename(distance=V1,best_fit=V2) %>% 
  mutate(replacement=judge[best_fit])
  

#Convert into a dataframe and find a decision rule for replacement
#Just keep the ones that change
out <- out %>% 
  mutate(replacement=ifelse(distance<0.06,replacement,judge)) %>% 
  filter(replacement!=judge)

#Replace judge names
judge_df2 <- judge_df %>%
  mutate(judge=plyr::mapvalues(judge,from=out$judge,to=out$replacement))

#How often did each judge REALLY appear?
rank_after <- judge_df2 %>% 
  dplyr::count(judge,sort=T) %>% 
  tibble() %>% 
  mutate(judge=ifelse(is.na(judge),"None",judge))


##Visualization


before <- rank_before %>%
  filter(n>20|str_detect(txt,"Aa|Geoff|Amanda|Chris Sa")) %>%
  mutate(dups=case_when(
    str_detect(txt,"Amanda") ~ "A",
    str_detect(txt,"Geoff") ~ "B",
    str_detect(txt,"Chris San") ~ "C",
    str_detect(txt,"Aa") ~ "D",
    TRUE ~ "E"
  )) %>% 
  ggplot(aes(x=n,y=reorder(txt,n),fill=dups))+
  geom_col()+
  scale_fill_manual(values=c("goldenrod","chartreuse4","cadetblue","violet","grey80"))+
  theme(legend.position = "none")+
  labs(x="Appearances in Shows",y="",title="Before",subtitle="Duplicates due to tiping errors")


after <- rank_after %>%
  filter(n>20) %>%
  mutate(dups=case_when(
    str_detect(txt,"Amanda") ~ "A",
    str_detect(txt,"Geoff") ~ "B",
    str_detect(txt,"Chris San") ~ "C",
    str_detect(txt,"Aa") ~ "D",
    TRUE ~ "E"
  )) %>% 
  ggplot(aes(x=n,y=reorder(txt,n),fill=dups))+
  geom_col()+
  scale_fill_manual(values=c("goldenrod","chartreuse4","cadetblue","violet","grey80"))+
  theme(legend.position = "none")+
  labs(x="Appearances in Shows",y="",title="After",subtitle="No fuzzy duplicates")



library(patchwork)

textcol <- "midnightblue"

before+after+plot_annotation(
  title="Fuzzy duplicates",
  subtitle="Replace fuzzy duplicates from the judge names using the stringdist package."
)&
  theme(plot.background = element_rect(fill = "ivory"),
        panel.background = element_rect(fill="ivory2"),
        axis.title = element_text(family = "sans" ,size=14,colour=textcol),
        axis.text = element_text(family = "sans" ,size=14,colour=textcol),
        plot.title = element_text(family = "sans", face = "bold", size = 20, colour = textcol),
        plot.subtitle = element_text(family = "sans" ,size=16, colour = textcol))
