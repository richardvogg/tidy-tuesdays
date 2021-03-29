library(ggplot2)
library(dplyr)

tuesdata <- tidytuesdayR::tt_load(2021, week = 14)

sysfonts::font_add_google(name = "Annie Use Your Telescope","Annie")
showtext::showtext_auto()

sephora <- tuesdata$sephora
ulta <- tuesdata$ulta
allShades <- tuesdata$allShades
allNumbers <- tuesdata$allNumbers
allCategories <- tuesdata$allCategories

top15 <- allCategories %>% 
  count(brand,sort=TRUE) %>% 
  slice(1:15) %>% 
  .$brand

sorted <- allCategories %>%
  group_by(brand) %>%
  arrange(brand,lightness) %>%
  mutate(rank=rank(lightness,ties.method = "first"))

get_brand_colors <- function(brand_name) {
  sorted %>%
    filter(brand==brand_name)
}

plot_brand_colors <- function(brand_data) {
  title <- brand_data[[1,1]]
  
  ggplot(brand_data,aes(x=rank,y=1,fill=hex)) + geom_tile() +
    scale_fill_manual(values=brand_data$hex) +
    labs(subtitle = title) +
    theme_void()+
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.subtitle = element_text(family="Annie",size=20))
}

plots <- lapply(top15,function(x) {
  x %>% 
     get_brand_colors() %>% 
     plot_brand_colors()
  })

library(patchwork)


(plots[[1]] + plots[[2]] + plots[[3]]) /
  (plots[[4]] + plots[[5]] + plots[[6]]) /
  (plots[[7]] + plots[[8]] + plots[[9]]) /
  (plots[[10]] + plots[[11]] + plots[[12]]) /
  (plots[[13]] + plots[[14]] + plots[[15]]) +
  plot_annotation(title = "The palettes of the brands with most Makeup Shades",
                  caption = "Data: The Pudding") &
  theme(plot.title = element_text(family = "Annie", size = 25),
        plot.caption = element_text(family="Annie", size = 12))

