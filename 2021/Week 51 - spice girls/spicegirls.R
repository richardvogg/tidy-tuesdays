library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyr)
library(ggchicklet) #for rounded bars
library(patchwork)
library(cowplot)

tuesdata <- tidytuesdayR::tt_load('2021-12-14')

lyrics <- tuesdata$lyrics


## Quiz

create_histogram <- function(title = "Wannabe") {
  word_count <- lyrics %>%
    unnest_tokens(word, line) %>%
    filter(song_name == title) %>%
    count(word, sort = TRUE) %>%
    filter(!word %in% c("you", "i", "we", "that"))
  
  rows <- nrow(word_count)
  
  top_3 <- word_count %>%
    mutate(rank = rank(-n,ties.method = "random")) %>%
    filter(rank < 4)
  
  max_occ <- top_3 %>%
    filter(rank == 1) %>%
    .$n
  
  
  
  top <- top_3 %>%
    ggplot(aes(y = n, x = reorder(word,n))) + geom_chicklet(radius = grid::unit(5, "pt")) +
    geom_text(aes(label = word, y = 0), hjust = 0, nudge_y = max_occ / 20, col = "white") +
    geom_text(data = subset(top_3, rank==1), aes(label = n, y = n), 
              col = "white", nudge_y = -max_occ / 20) +
    coord_flip() +
    theme_void()
  
  tail <- word_count %>%
    ggplot(aes(x = n, y = reorder(word,n))) + geom_col(width = 1) +
    annotate("text", x = max_occ / 3, y = rows/2, label = paste(rows, "other words")) +
    coord_cartesian(ylim = c(1, rows-3.2)) +
    theme_void()
  
  
  output <- top / tail + plot_layout(heights = c(0.3, 0.7))
  return(output)
}

set.seed(83)

titles <- sample(unique(lyrics$song_name), 3)

out <- lapply(titles, 
       function(x) create_histogram(x))



title <- ggdraw() + 
  draw_label(
    "Can you guess the three songs by the Spice Girls?",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plot_row <- plot_grid(out[[1]], out[[2]], out[[3]], nrow = 1)

plot_grid(title, plot_row, ncol = 1, rel_heights = c(0.1, 1))


### Wannabe


