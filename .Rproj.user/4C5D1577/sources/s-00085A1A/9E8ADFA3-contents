library(tidyverse)
library(stringdist)
library(ggrepel)

tuesdata <- tidytuesdayR::tt_load(2020, week = 43)

beer <- tuesdata$beer_awards

#Just Breweries (independent of cities)
beer %>% count(brewery,sort=T) %>% View()


#Find fuzzy duplicates with this function
remove_duplicates <- function(vec) {
  wordcount <- vec %>%
    tibble(txt=vec) %>% 
    dplyr::count(txt,sort=T)
  
  words <- wordcount$txt
  
  out <- sapply(seq_along(words)[-1],function(i) {
    dist2 <- stringdist(words[i],words[1:i-1],method='jw',p=0.1)
    #dist2 <- stringdist(words[i],words[1:i-1],method='lv')/nchar(words[i])
    best_fit <- which.min(dist2)
    similarity2 <- min(dist2)
    return(c(similarity2,best_fit))
  }) %>% 
    t() %>%
    as.data.frame() %>% 
    add_row(V1=1,V2=1,.before = 1) %>% 
    cbind(wordcount) %>% 
    dplyr::rename(distance=V1,best_fit=V2) %>% 
    mutate(replacement=txt[best_fit])
  
  return(out)
}



out <- beer %>% 
  rowwise() %>%
  mutate(city_brewery=paste(city,brewery,sep=";")) %>%
  .$city_brewery %>% 
  
  #create the dataframe for decision making
  remove_duplicates() %>% 
  separate(txt,into = c("city1","brewery_orig"),sep = ";") %>%
  separate(replacement,into=c("city2","brewery_repl"),sep=";") %>% 
  
  #remove breweries with similar names but in different cities
  filter(city1==city2) %>%
  mutate(dist=stringdist(brewery_orig,brewery_repl,method="jw"))


#Make a dictionary with replacements, if the names are not too distinct
dict <- out %>% 
  mutate(brewery_repl=ifelse(dist<=0.15,brewery_repl,brewery_orig)) %>%
  filter(brewery_orig!=brewery_repl) 


#replace fuzzy duplicates
beer$brewery2 <- plyr::mapvalues(beer$brewery,from=dict$brewery_orig,to=dict$brewery_repl)

#Top 10 before
top10 <- beer %>% count(brewery,city,state,sort=T)
  
#Top 10 after
top10b <- beer %>% count(brewery2,city,state,sort=T)


#####
#Visualization

before <- top10 %>% slice(1:15) %>%
  mutate(deschutes=ifelse(brewery=="Deschutes Brewery","Y","N")) %>%
  ggplot(aes(x=n,y=reorder(brewery,n),fill=deschutes))+
    geom_col()+
    geom_text(aes(label=n,x=n-5),col="white",size=5)+
    geom_text(aes(label=state,x=5),col="white",size=4)+
  scale_fill_manual(values=c("grey40","goldenrod"))+
  theme(legend.position = "none",
        axis.text.x = element_blank())+
  labs(x="Number medals",y="",title="Before",subtitle="Original registered names")

after <- top10b %>% slice(1:15) %>%
  mutate(deschutes=ifelse(brewery2=="Deschutes Brewery","Y","N")) %>%
  ggplot(aes(x=n,y=reorder(brewery2,n),fill=deschutes))+
    geom_col()+
    geom_text(aes(label=n,x=n-5),col="white",size=5)+
  geom_text(aes(label=state,x=5),col="white",size=4)+
  scale_fill_manual(values=c("grey40","goldenrod"))+
  theme(legend.position = "none",
        axis.text.x = element_blank())+
  labs(x="Number medals",y="",title="After",subtitle="No fuzzy duplicates")

deschutes <- beer %>%
  filter(brewery2=="Deschutes Brewery") %>% 
  mutate(medal_num=case_when(medal=="Gold" ~ 3,
                             medal=="Silver" ~ 2,
                             medal == "Bronze" ~ 1)) %>%
  ggplot(aes(x=year,y=medal_num,label=beer_name))+
  geom_segment(aes(x=1989,xend=2021,y=0,yend=0))+
  geom_segment(aes(xend=year,yend=0,col=brewery),size=1.5)+
  geom_point(size=2)+
  #geom_text_repel(angle=90,size=3,min.segment.length = 0)+
  scale_y_continuous(breaks=1:3,labels = c("Bronze","Silver","Gold"))+
  labs(x="",y="",title="Why?",subtitle="Two different names",col=NULL)+
  theme(legend.position = c(0.8,0.7))
  
#Combining the plots

library(patchwork)

textcol <- "midnightblue"

((before | after) / deschutes )+
  plot_annotation(
  title="Top Ten Breweries with most medals in the Great American Beer Festival",
  subtitle="Deschutes Brewery was registered under a slightly different name before 2000. \nRemoving fuzzy duplicates we see that it is actually among the top 10 breweries which won most medals."
)+
  plot_layout(heights=c(0.7,0.3))&
  theme(plot.background = element_rect(fill = "ivory"),
        panel.background = element_rect(fill="ivory2"),
        axis.title = element_text(family = "sans" ,size=14,colour=textcol),
        axis.text = element_text(family = "sans" ,size=14,colour=textcol),
        plot.title = element_text(family = "sans", face = "bold", size = 20, colour = textcol),
        plot.subtitle = element_text(family = "sans" ,size=16, colour = textcol))


