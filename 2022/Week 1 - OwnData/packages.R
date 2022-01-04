library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(stringr)
library(ggwordcloud)

sysfonts::font_add_google(name = "Nunito", "Nunito")
showtext::showtext_auto()

path <- "/Users/vogg/Documents/R/tidy-tuesdays"

all_files <- list.files(path = path, pattern =".R$", recursive = TRUE)
files <- paste0(path, "/" , all_files)
file_lines <- map(files, readLines)


df <- map_dfr(file_lines, ~ tibble(package = str_extract_all(.x, "^library(([^)]+))")),
                  .id = "id") %>%
  unnest(package) %>%
  filter(!is.null(package)) %>%
  mutate(package = str_replace(package, "library\\(",""))


  
df %>%
  add_count(package) %>%
  group_by(package) %>%
  mutate(first = row_number(package),
         first_app = min(as.numeric(id))) %>% 
  mutate(first = ifelse(first == 1, package, NA)) %>%
  ggplot(aes(x = as.numeric(id), y = reorder(package, -first_app+n/1000),
             fill = log(n))) + 
  geom_point(pch = 23, size = 4) +
  geom_text(aes(label = first), size = 5, hjust = 1, nudge_x = -0.7, family = "Nunito") +
  annotate("text", -4, 14, label = "#TidyTuesday", size = 20, 
           fontface = "bold", color = "black", hjust = 0, family = "Nunito") +
  annotate("text", -4, 8, label = "R packages I used in 38 weeks", size = 10, 
           fontface = "bold", color = "black", hjust = 0, family = "Nunito") +
  scale_fill_gradient(low = "grey70", high = "#FF7800",labels = c("Just once", "","","Frequently")) +
  labs(fill = "") +
  expand_limits(x = -5) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey80", size = 0.1),
        legend.position = c(0.2, 0.1),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, family = "Nunito"))
  


ggsave("2022/Week 1 - OwnData/plot.png", width = 15, height = 9)

#Barchart
df %>%
  count(package) %>%
  ggplot(aes(x = n, y = reorder(package, n))) + geom_col()

#Wordcloud
df %>%
  count(package) %>%
  ggplot(aes(label = package, size = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()
