#### COVID cases Germany

library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(stringdist) # For Fuzzy merging
library(TTR) # Moving Averages/Sums

# Complete data from
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0?selectedAttribute=Refdatum

options(scipen = 10)

dates <- seq.Date(from=as.Date("2020-02-01"),to=as.Date("2020-11-25"),by="day")

kreise <- read_sf("/Users/vogg/Documents/R/datasets/Kreisgrenzen_2017_mit_Einwohnerzahl/Kreisgrenzen_2017_mit_Einwohnerzahl.shp") %>%
  mutate(name=paste(ifelse(BEZ %in% c("Kreisfreie Stadt","Stadtkreis"),"SK","LK"),GEN)) %>%
  st_simplify(dTolerance=1000)

kreise_df <- st_drop_geometry(kreise)


df <- expand.grid(
  date=dates,name=kreise$name
) %>%
  inner_join(select(kreise_df,name,EWZ),by="name")

cases <- read.csv("/Users/vogg/Documents/R/datasets/RKI_COVID19.csv") %>% 
  group_by(Landkreis,Meldedatum) %>%
  summarise(cases=sum(AnzahlFall)) %>%
  ungroup()

cleaner <- function(vec) {
  vec %>% tolower() %>%
    str_remove("eifelkreis|regionalverband|stadtverband|saale|am main") %>%
    return()
}



fuzzy_matches <- function(clean_vec,dirty_vec) {
  control <- data.frame(original=dirty_vec)
  
  distmatrix <- stringdist::stringdistmatrix(cleaner(clean_vec),cleaner(dirty_vec),method='jw',p=0.1)
  best_fit <- apply(distmatrix,2,which.min) %>% as.integer()
  similarity <- apply(distmatrix,2,min)
  
  control$best_fit <- clean_vec[best_fit]
  control$similarity <- similarity 
  
  return(control)
}

dict <- fuzzy_matches(kreise_df$name,unique(cases$Landkreis))


final <- cases %>%
  #fuzzy merging
  mutate(LK_new=plyr::mapvalues(Landkreis,from=dict$original,to=dict$best_fit)) %>% 
  group_by(LK_new,Meldedatum) %>%
  summarise(cases=sum(cases)) %>% 
  ungroup() %>%
  transmute(LK_new,date=as.Date(substr(Meldedatum,1,10)),cases) %>%
  #Fill all dates (even if there were no cases reported)
  right_join(df,by=c("LK_new"="name","date")) %>%
  mutate(cases=ifelse(is.na(cases),0,cases)) %>% 
  arrange(LK_new,date) %>%
  #Moving average 7 days
  group_by(LK_new) %>%
  mutate(avg_cases_7_day = runSum(cases, 7)) %>%
  ungroup() %>%
  mutate(incidence_per_100k = 100000*avg_cases_7_day/EWZ) %>%
  mutate(month=format(date,"%B")) %>%
  #Get the geometries from kreise
  left_join(select(kreise,name),by=c("LK_new"="name")) %>%
  mutate(cases7_per_bin=cut(incidence_per_100k,breaks=c(-1,4,25,50,100,250,1000),
                            labels=c("<5","5 to 25","25 to 50",
                                     "50 to 100","100 to 250","over 250"))) %>%
  select(-geometry)


final %>%
  filter(date=="2020-10-01") %>%
  mutate(rank_max=rank(incidence_per_100k)) %>%
  ggplot(aes(x=EWZ,y=incidence_per_100k))+geom_point()
  

  
final %>%
  mutate(ewz_bin=EWZ>350000) %>%
    filter(date %in% as.Date(c("2020-10-01", "2020-10-15", "2020-10-30", "2020-11-15"))) %>%
  ggplot(aes(y = EWZ, x = incidence_per_100k, col = ewz_bin)) +
  geom_point() +
  geom_text_repel(aes(label = LK_new), size = 3) +
  scale_y_log10() +
  labs(title = "Extreme results are more likely to happen when sample sizes are smaller.",
       x = "Incidence (per 100k)", y = "Inhabitants",
       color = "City size > 350000") +
  facet_wrap(~date, scales = "free_x")

ggsave("2022/Week 3 - Chocolate/covid_example.png", width = 11, height = 7)

sel_dates <- seq.Date(from=as.Date("2020-10-01"),to=as.Date("2020-11-25"),length.out = 16) %>%
  as.character()

final %>%
  mutate(ewz_bin=ifelse(EWZ>350000,"2_large","1_small")) %>%
  mutate(date=as.character(date)) %>%
  filter(date %in% sel_dates) %>%
  ggplot(aes(x=ewz_bin,y=incidence_per_100k,col=ewz_bin))+geom_boxplot()+
  facet_wrap(~date)
