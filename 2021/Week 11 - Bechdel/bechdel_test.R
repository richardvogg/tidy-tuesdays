library(ggplot2)
library(dplyr)
library(ggstream)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load(2021, week = 11)

sysfonts::font_add_google(name = "Chango","Chango")
sysfonts::font_add_google(name = "Roboto","Roboto")
showtext::showtext_auto()

raw <- tuesdata$raw_bechdel
movies <- tuesdata$movies



actors_df <- movies %>%
  select(imdb,actors) %>%
  tidyr::separate_rows(actors,sep=", ") %>%
  left_join(movies %>% select(imdb,clean_test),by="imdb") %>%
  group_by(actors) %>%
  summarise(n=n(),
            ok = sum(clean_test=="ok"),
            dubious = sum(clean_test=="dubious"),
            talk_only_about_men = sum(clean_test=="men"),
            no_talk = sum(clean_test=="notalk"),
            no_women = sum(clean_test=="nowomen")) %>%
  mutate(perc_pass = round(ok/n,3),
         perc_fail = 1-perc_pass) %>%
  filter(n>7)

pass_actors <- actors_df %>%
  arrange(desc(perc_pass),desc(n)) %>%
  top_n(20,perc_pass) %>%
  mutate(i = row_number(),type="pass",perc=perc_pass) 

fail_actors <- actors_df %>%
  arrange(desc(perc_fail),desc(n)) %>%
  top_n(20,perc_fail) %>%
  mutate(i = row_number(),type="fail",perc=perc_fail)

pass_actors %>%
  rbind(fail_actors) %>%
  ggplot(aes(x=1,y=-i))+
  geom_text(aes(label=actors,alpha=perc,col=type),family="Chango")+
  scale_alpha_binned(range=c(0.5,1))+
  scale_color_manual(values=c("darkred","blue"))+
  labs(title="Who participates in movies which pass / fail the Bechdel test?",
       subtitle=paste0("For a movie to pass the test there have to be at least two named women and they",
                       "\nhave to have a conversation with each other which is not about men.",
                       "\n\nThese lists show actors / actresses which appear in at least eight movies (1970-2013)",
                       "\nand almost exclusively participate in movies which pass or fail the test."),
       caption="Data: FiveThirtyEight")+
  facet_wrap(~type)+
  theme_void()+
  theme(legend.position = "none",
    plot.title=element_text(family="Roboto",size=16,hjust=0.1),
    plot.subtitle = element_text(family="Roboto",size=12,hjust=0.1,margin=margin(10,0,10,0)),
    plot.caption = element_text(family="Roboto",size=8),
    plot.background = element_rect(fill = "ivory",colour=NA),
    strip.text.x = element_text(family = "Roboto",size=15,margin=margin(10,0,5,0)))


## Other ideas


movies_df <- movies %>%
  group_by(year,clean_test) %>%
  summarise(n=n(),mean_budget=mean(budget)) %>%
  mutate(clean_test_text = factor(case_when(
    clean_test == "dubious" ~ "Dubious",
    clean_test == "men" ~ "Women talk only about men",
    clean_test == "notalk" ~ "Women don't talk to each other",
    clean_test == "nowomen" ~ "Fewer than two women",
    clean_test == "ok" ~ "Passes Bechdel Test"
  )))

## ggstream

stream <- movies_df %>%
  mutate(clean_test_text=forcats::fct_relevel(clean_test_text,"Women talk only about men","Fewer than two women",
                  "Women don't talk to each other","Dubious","Passes Bechdel Test")) %>%
  ggplot(aes(year, n, fill = clean_test_text)) +
  geom_stream()

budget <- movies %>%
  mutate(clean_test_text = factor(case_when(
    clean_test == "dubious" ~ "Dubious",
    clean_test == "men" ~ "Women talk only about men",
    clean_test == "notalk" ~ "Women don't talk to each other",
    clean_test == "nowomen" ~ "Fewer than two women",
    clean_test == "ok" ~ "Passes Bechdel Test"
  ))) %>%
  mutate(clean_test_text=forcats::fct_relevel(clean_test_text,"Women talk only about men","Fewer than two women",
                                              "Women don't talk to each other","Dubious","Passes Bechdel Test")) %>%
  ggplot(aes(x=forcats::fct_rev(clean_test_text),y=budget,fill=clean_test_text))+
  geom_violin()+
  coord_flip()


library(patchwork)

stream + budget &
  theme(legend.position="none")
