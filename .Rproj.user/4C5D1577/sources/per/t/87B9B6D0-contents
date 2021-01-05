#remotes::install_github("wilkelab/ggtext")

library(tidyverse)
library(tidytext)
library(ggtext)
library(ggwordcloud)
library(patchwork)

sysfonts::font_add_google(name = "Roboto Slab","Roboto")
showtext::showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2020, week = 37)

friends_df <- tuesdata$friends

#Stopwords and digits
stopwords_df <- rbind(stop_words,data.frame(word=as.character(1:9),lexicon="other"))

#Find the main characters' names
main_chars <- friends_df %>%
  count(speaker,sort=T) %>%
  slice(1:6) %>%
  .$speaker

#Count frequency of words
word_df <- friends_df %>%
  filter(speaker %in% main_chars) %>%
  unnest_tokens(word,text) %>%
  anti_join(stopwords_df,by="word") %>%
  count(speaker,word,sort=T) 


#Rank the words by who uses them most
ranked_words <- word_df %>% 
  filter(n>1) %>%
  group_by(word) %>%
  mutate(rank=rank(-n,ties.method = "first"))

#Calculate the ratio between the person who uses the word most and the second most
cloud_words <- ranked_words %>%
  filter(rank<=2) %>%
  pivot_wider(id_cols=word,names_from=c(rank),values_from=c(n,speaker)) %>% 
  filter(n_1>14) %>%
  mutate(n_2=ifelse(is.na(n_2),1,n_2),
         ratio=n_1/n_2) %>% 
  arrange(desc(ratio)) %>%
  filter(ratio>2) %>%
  mutate(type=case_when(
    word %in% c("bing","joe","janice","joey's",
                "eddie","kathy","tribbiani",
                "pete","michelle","mike","mike's",
                "ursula","frank","david","alice",
                "jack","gavin","joanna","mindy",
                "amy","paolo","barry","elizabeth",
                "susan","carol","ben","emily","mona",
                "joshua","remoray","drake","estelle","buffay") ~ "person",
    word %in% c("actor","audition","director","acting",
                "restaurant","chef","kitchen","paleontology",
                "professor","student","evolution",
                "ralph","lauren","shoe","purse","copy",
                "assistant","massage","client","guest","desk",
                "museum") ~ "job",
    TRUE ~ "other"
  ))

#Visualizations

textcol <- "midnightblue"


clouds <- cloud_words %>% 
  ggplot()+
    geom_text_wordcloud(aes(label=word,size=sqrt(n_1),col=type),family="Roboto")+
    scale_size_area(max_size=9)+
    guides(size=FALSE)+
    facet_wrap(~speaker_1)+
  labs(title="Typical words (not most frequent words!)",
  subtitle="<span style='font-size:14pt'>Colored by 
    <span style='color:goldenrod;'>job related </span>, 
    <span style='color:chartreuse4;'>person related </span>, and
  <span style='color:darkorchid1;'>others </span>
    </span>")+
  scale_color_manual(values=c("goldenrod","darkorchid1","chartreuse4"))+
  theme(plot.background = element_rect(fill = "ivory"),
        panel.background = element_rect(fill="ivory2"),
        text=element_text(family="Roboto",size=14,colour=textcol),
        plot.title = element_text(family = "Roboto", face = "bold", size = 16, colour = textcol),
        plot.subtitle = element_markdown(lineheight = 1.1))

png(here::here("Week 37 - Friends/typical_words.png"),width = 1100,height=500)
clouds
dev.off()


audition <- ranked_words %>%
  filter(word=="audition") %>% 
  ggplot(aes(x=n,y=speaker))+
  geom_col()+
  geom_vline(xintercept = 25,col="red")+
  labs(x="Appearances of the word audition",y="",title="Audition - a Joey word")


wow <- ranked_words %>%
  filter(word=="wow") %>% 
  ggplot(aes(x=n,y=speaker))+
  geom_col()+
  geom_vline(xintercept=83,col="red")+
  labs(x="Appearances of the word wow",y="",title="Wow - not (only) a Rachel word")



png(here::here("Week 37 - Friends/example.png"),width = 1000,height=400)

audition + wow +
  plot_annotation(
    title="Definition and Example",
    subtitle="A typical word for a character is a word that is used by this character more than double as often as by any other character."
  )&
  theme(plot.background = element_rect(fill = "ivory"),
        panel.background = element_rect(fill="ivory2"),
        text = element_text(family="Roboto",size=12,colour=textcol),
        plot.title = element_text(family = "Roboto", face = "bold", size = 16, colour = textcol))

dev.off()



#Other


#Check tf-idf
word_df %>%
  filter(n>14) %>%
  bind_tf_idf(word,speaker,n) %>% 
  group_by(speaker) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~speaker, ncol = 2, scales = "free") +
  coord_flip()


#Check log-odds

library(tidylo)

word_df %>%
  filter(n>14) %>%
  bind_log_odds(speaker,word,n) %>%
  group_by(speaker) %>%
  top_n(10,log_odds_weighted) %>%
  ungroup() %>%
  mutate(word=reorder_within(word,log_odds_weighted,speaker)) %>%
  ggplot(aes(x=word, log_odds_weighted, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~speaker, ncol = 2, scales = "free_y") +
  coord_flip()+
  scale_x_reordered()
