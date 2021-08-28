library(dplyr)
library(ggraph)
library(igraph)
library(graphlayouts)

sysfonts::font_add_google(name = "Jura", "Jura")
showtext::showtext_auto()

lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

taxon <- c("CMED", "DMAD", "EALB", "ECOL", "ECOR", "EFLA", "EFUL", "EMAC",
           "EMON", "ERUB", "ERUF", "ESAN", "EUL", "GMOH", "HGG", "LCAT",
           "LTAR", "MMUR", "MZAZ", "NCOU", "NPYG", "OGG", "PCOQ", "PPOT",
           "VAR", "VRUB", "VVV")
taxon_name <- c("Fat-tailed dwarf lemur", "Aye-aye", "White-fronted brown lemur",
                "Collared brown lemur", "Crowned lemur", "Blue-eyed black lemur",
                "Common brown lemur", "Black lemur", "Mongoose lemur",
                "Red-bellied lemur", "Red-fronted brown lemur",
                "Sanford’s brown lemur", "hybrid", "Mohol bushbaby",
                "Eastern lesser bamboo lemur", "Ring-tailed lemur",
                "Slender loris", "Gray mouse lemur", "Northern giant mouse lemur",
                "Slow loris", "Pygmy slow loris", "Northern greater galago",
                "Coquerel’s sifaka", "Potto", "Varecia	hybrid",
                "Red ruffed lemur", "Black-and-white ruffed lemur")

colors <- rep(RColorBrewer::brewer.pal(12, "Set3"), 3)[1:27]



make_graph <- function(taxon_short = "OGG") {
  tree <- lemurs %>%
    distinct(taxon, dlc_id, lemur_name = name, dam_id, dam_dob, 
             sire_id, sire_dob, sex, dob) %>%
    filter(taxon == taxon_short)
  
  
  edges <- tree %>%
    distinct(from = dam_id, to = dlc_id) %>%
    rbind(tree %>% distinct(from = sire_id, to = dlc_id))
  
  vertices <- tree %>%
    distinct(name = dlc_id, lemur_name, dob, sex) %>%
    rbind(tree %>% distinct(name = sire_id, lemur_name = NA, dob = sire_dob, sex = NA)) %>%
    rbind(tree %>% distinct(name = dam_id, lemur_name = NA, dob = dam_dob, sex = NA)) %>%
    group_by(name) %>%
    summarise(lemur_name = max(lemur_name, na.rm = TRUE),
              dob = max(dob, na.rm = TRUE),
              sex = max(sex, na.rm = TRUE))
  
  g <- graph_from_data_frame(edges, vertices = vertices)
  g <- simplify(g)
  g <- as.undirected(g)
  
  return(list(edges = edges, vertices = vertices, g=g))
}


for(i in 1:length(taxon)) {
  family <- make_graph(taxon[i])
  
  bb <- layout_as_backbone(family$g, keep = 0.4)
  
  path <- "C:/Users/Richard/Documents/GitHub/tidy-tuesdays/"
  
  
  
  ggraph(family$g,layout = "manual", x = bb$xy[,1],
         y = as.numeric(format(family$vertices$dob,"%Y"))) +
    geom_edge_link0(edge_width = 0.1, alpha = 0.2)+
    geom_node_point(aes(col = family$vertices$sex))+
    geom_node_text(aes(label = family$vertices$lemur_name), family = "Jura",
                   size = 7, check_overlap = TRUE, nudge_y = -0.4) +
    labs(title = paste("Family tree of", taxon_name[i]),
         color = "Sex",
         caption = "Data: Duke Lemur Center") +
    theme_graph() +
    theme(axis.text.y = element_text(family = "Jura", size = 30),
          axis.ticks.y = element_line(),
          plot.title = element_text(family = "Jura", size = 50),
          plot.caption = element_text(family = "Jura", size = 25),
          legend.title = element_text(family = "Jura", size = 30),
          legend.text = element_text(family = "Jura", size = 25),
          panel.background = element_rect(fill = colors[i], colour = "black", size = 1))
  
  ggsave(paste0(path,"2021/Week 35 - lemurs/plot",taxon[i],".png"), 
         dpi = 300, height = 7, width = 7, units = "in")
  
  print(i)
  
}
