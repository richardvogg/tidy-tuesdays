library(tidyverse)
library(metamer)
library(magick)

tuesdata <- tidytuesdayR::tt_load(2020, week = 42)

datasaurus <- tuesdata$datasaurus

mews <- image_read("Week 42 - datasauRus/Mews2.jpg")

#convolution filters to detect edges
edge <- matrix(c(0,1,0, 1,-4,1, 0,1,0), 3, 3)
sharpen <- matrix(c(0,-1,0, -1,4,-1, 0,-1,0),3, 3)

#Image processing
mews_borders <- mews %>% 
  image_quantize(colorspace="gray") %>%
  image_convolve(edge) %>% 
  image_convolve(sharpen) %>%
  image_threshold("20%",type="white") %>% 
  image_threshold("100%",type="black") %>% 
  image_median(2) %>% 
  #crop to the head of the cat
  image_crop("140x140+60+25") %>% 
  #transform into tidy dataframe
  image_data() %>%
  as.integer() %>%
  {.[,,1]} %>%
  as.data.frame() %>%
  mutate(y=row_number()) %>%
  pivot_longer(-y,names_to="x") %>%
  mutate(x=parse_number(x),
         y=120-y)

#Plot Mews (not yet equal to datasaurus)
mews_borders %>%
  filter(value==255) %>% 
  ggplot(aes(x,y))+geom_point()
  

dino <- datasaurus %>% filter(dataset=="dino") %>% select(x,y)

mews <- mews_borders %>%
  filter(value==255) %>%
  select(x,y)

# This is an awesome feature, but my drawings always looked terrible
final_mews <- draw_data(mews) %>% select(x,y) %>%
  mutate(x=100*(x-min(x))/(max(x)-min(x)),
         y=100*(y-min(y))/(max(y)-min(y)))


final_mews %>% ggplot(aes(x,y))+geom_point()

#Check mean and sd and compare with dino
final_mews %>%
  summarise(mean(x),sd(x),mean(y),sd(y),cor(x,y))

dino %>%
  summarise(mean(x),sd(x),mean(y),sd(y),cor(x,y))

#Adapt means and standard deviation to be almost equal to dino
final_mews_adapt <- final_mews %>%
  mutate(x=x+9,y=y-12,
         x=20+x*0.6)

final_mews_adapt %>%
  summarise(mean(x),sd(x),mean(y),sd(y),cor(x,y))

dino %>% ggplot(aes(x,y))+geom_point()

final_mews_adapt %>% ggplot(aes(x,y))+geom_point()

#This function uses simulated annealing to move points from the dino constellation
#towards mews, preserving mean, sd and cor.
#Higher N (>500000) yield better results in this case but run a long time.
metamers <- metamerize(data=dino,
           preserve = delayed_with(mean(x), sd(x), mean(y), sd(y), cor(x, y)),
           minimize = c(mean_dist_to(final_mews_adapt),mean_self_proximity),
           N=250000,
           trim=1000)

metamers[[length(metamers)]] %>%
  ggplot(aes(x,y))+geom_point()


metamers[[length(metamers)]] %>%
  summarise(mean(x),sd(x),mean(y),sd(y),cor(x,y))
  
dino %>%
  summarise(mean(x),sd(x),mean(y),sd(y),cor(x,y))





### Other ideas


library(raster)
data("cryptomeria")
mews <- raster::brick("Week 42 - datasauRus/Mews2.jpg")



names(mews)<-c("R","G","B")
dropLayer(mews,c("G","B"))


tidy_mews <- mews %>% 
  rasterToPoints() %>% 
  data.frame() %>%
  transmute(x=x/4,y=y/3,val=round(0.3*R+0.59*G+0.11*B))

tidy_mews %>%
  ggplot(aes(x=x,y))+
  geom_tile(aes(fill=val))+
  scale_fill_gradient(low="black",high="white")

draw_data(tidy_mews)

plotRGB(mews,r=3,g=2,b=1, stretch = "lin")
