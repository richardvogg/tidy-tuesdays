library(ggplot2)
library(dplyr)
library(cowplot)

sysfonts::font_add_google(name = "Public Sans", "Public")
showtext::showtext_auto()

options(scipen = 10)

colornames <- c("black", "brown", "tan", "gold", "pink", "crimson", "green", "blue")
colors <- c("#000000", "#654321", "#d2b48c", "#ffd700", "#ffc0cb", "#dc143c", "#00aa00", "#4682b4")

year <- c(1875, 1880, 1885, 1890, 1895, 1899)
valuation <- c("5,393,885", "5,764,293","8,153,390", "12,322,003", "12,941,230", "13,447,423")
valuation_num <- c(5393885, 5764293, 8153390, 12322003, 12941230, 13447423)
diff <- valuation_num - lag(valuation_num, default = 0)
width <- c(10, 1, 5, 8, 1, 1)
width <- diff
ypos <- rev(cumsum(rev(width)))

df <- data.frame(year, valuation, width, ypos)

df2 <- data.frame(1:3)

circle <- ggplot(df, aes(x = 1, y = width, fill = factor(year))) +
  geom_col() +
  #geom_rect(aes(xmin = 0.75, xmax = 0.8, ymin = 16, ymax = 20), fill = "grey60") +
  scale_fill_manual(values = c("black", "grey60", "#4682b4", "#ffd700", "grey80", "#dc143c")) +
  geom_polygon(data = df2, aes(x = c(1.13, 1.165, 1.15), y = c(ypos[2], ypos[2], 0.8*ypos[1])), fill = "grey60") +
  
  geom_polygon(data = df2, aes(x = c(0.88, 0.92, 0.90), y = c(ypos[2], ypos[2], 0.8*ypos[1])), fill = "grey60") +
  geom_polygon(data = df2, aes(x = c(0.88, 0.92, 0.90), y = c(ypos[3], ypos[3], 0.8*ypos[1])), fill = "#4682b4") +
  
  geom_polygon(data = df2, aes(x = c(0.76, 0.79, 0.775), y = c(ypos[2], ypos[2], 0.8*ypos[1])), fill = "grey60") +
  geom_polygon(data = df2, aes(x = c(0.76, 0.79, 0.775), y = c(ypos[3], ypos[3], 0.8*ypos[1])), fill = "#4682b4") +
  geom_polygon(data = df2, aes(x = c(0.76, 0.79, 0.775), y = c(ypos[4], ypos[4], 0.8*ypos[1])), fill = "#ffd700") +
  
  geom_polygon(data = df2, aes(x = c(0.63, 0.66, 0.645), y = c(ypos[2], ypos[2], 0.8*ypos[1])), fill = "grey60") +
  geom_polygon(data = df2, aes(x = c(0.63, 0.66, 0.645), y = c(ypos[3], ypos[3], 0.8*ypos[1])), fill = "#4682b4") +
  geom_polygon(data = df2, aes(x = c(0.63, 0.66, 0.645), y = c(ypos[4], ypos[4], 0.8*ypos[1])), fill = "#ffd700") +
  geom_polygon(data = df2, aes(x = c(0.63, 0.66, 0.645), y = c(ypos[5], ypos[5], 0.8*ypos[1])), fill = "grey80") +
  
  geom_polygon(data = df2, aes(x = c(1.33, 1.36, 1.345), y = c(ypos[2], ypos[2], 0.8*ypos[1])), fill = "grey60") +
  geom_polygon(data = df2, aes(x = c(1.33, 1.36, 1.345), y = c(ypos[3], ypos[3], 0.8*ypos[1])), fill = "#4682b4") +
  geom_polygon(data = df2, aes(x = c(1.33, 1.36, 1.345), y = c(ypos[4], ypos[4], 0.8*ypos[1])), fill = "#ffd700") +
  geom_polygon(data = df2, aes(x = c(1.33, 1.36, 1.345), y = c(ypos[5], ypos[5], 0.8*ypos[1])), fill = "grey80") +
  geom_polygon(data = df2, aes(x = c(1.33, 1.36, 1.345), y = c(ypos[6], ypos[6], 0.8*ypos[1])), fill = "#dc143c") +
  
  scale_y_reverse() +
  coord_polar(theta = "x") +
  theme_void() +
  theme(legend.position = "none")

ggdraw(circle) +
  #Valuations
  draw_label(paste0("$", df$valuation[1]), x = 0.5, y = 0.5, 
             colour = "white", size = 8, fontfamily = "Public") +
  draw_label(paste0("$", df$valuation[2]), x = 0.425, y = 0.43, 
             angle = 30, colour = "black", size = 6, fontfamily = "Public") +
  draw_label(paste0("$", df$valuation[3]), x = 0.57, y = 0.37, 
             angle = 310, colour = "white", size = 8, fontfamily = "Public") +
  draw_label(paste0("$", df$valuation[4]), x = 0.65, y = 0.5, 
             colour = "black", size = 8, fontfamily = "Public") +
  draw_label(paste0("$", df$valuation[5]), x = 0.63, y = 0.76, 
             angle = 50, colour = "black", size = 8, fontfamily = "Public") +
  draw_label(paste0("$", df$valuation[6]), x = 0.35, y = 0.75, 
             angle = 315, colour = "black", size = 8, fontfamily = "Public") +
  #Years
  draw_label(df$year[1], x = 0.5, y = 0.36, 
             colour = "white", size = 8, fontfamily = "Public") +
  draw_label(df$year[2], x = 0.5, y = 0.335, 
             colour = "black", size = 8, fontfamily = "Public") +
  draw_label(df$year[3], x = 0.5, y = 0.27, 
             colour = "white", size = 8, fontfamily = "Public") +
  draw_label(df$year[4], x = 0.5, y = 0.15, 
             colour = "black", size = 8, fontfamily = "Public") +
  draw_label(df$year[5], x = 0.5, y = 0.125, 
             colour = "black", size = 8, fontfamily = "Public") +
  draw_label(df$year[6], x = 0.5, y = 0.11, 
             colour = "black", size = 8, fontfamily = "Public") +
  #Title
  draw_label("ASSESSED VALUATION OF ALL TAXABLE PROPERTY \n OWNED BY GEORGIA NEGROES .", 
             x = 0.5, y = 0.95, hjust = 0.5, lineheight = 0.9,
             colour = "black", size = 14, fontfamily = "Public", fontface = "bold") +
  draw_label("Data: #DuBoisChallenge2022, Chart recreated using R", 
             x = 0.95, y = 0.05, hjust = 1,
             colour = "black", size = 10, fontfamily = "Public")

ggsave("2022/Test/plot.png", width = 11, height = 7)

