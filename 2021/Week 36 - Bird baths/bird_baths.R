#devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)

library(ggplot2)
library(ggradar)
library(dplyr)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load(2021, week = 36)

bird_baths <- tuesdata$bird_baths

bird_baths <- bird_baths %>%
  mutate(bioreg_short = case_when(
    bioregions == "Brigalow Belt South" ~ "BBS",
    bioregions == "Flinders Lofty Block" ~ "FLB",
    bioregions == "NSW North Coast" ~ "NNC",
    bioregions == "NSW South Western Slopes" ~ "NSS",
    bioregions == "South East Coastal Plain" ~ "SCP",
    bioregions == "South Eastern Highlands" ~ "SEH",
    bioregions == "South Eastern Queensland" ~ "SEQ",
    bioregions == "Southern Volcanic Plain" ~ "SVP",
    bioregions == "Sydney Basin" ~ "SYB",
    bioregions == "Victorian Midlands" ~ "VIM",
    bioregions == "Victorian Volcanic Plain" ~ "VVP"
  ))

# find birds with large urban / rural differences

urban_rural_ratios <- bird_baths %>%
  filter(!is.na(urban_rural)) %>%
  count(bird_type, urban_rural, wt = bird_count, sort = TRUE) %>%
  tidyr::pivot_wider(id_cols = bird_type, names_from = urban_rural, values_from = n) %>%
  mutate(ratio = Urban/Rural, total_count = Urban + Rural) %>%
  filter(total_count > 90)

urban3 <- urban_rural_ratios %>% top_n(3, ratio) %>% .$bird_type
rural3 <- urban_rural_ratios %>% top_n(3, -ratio) %>% .$bird_type

# make dataset ready for radar chart

total <- bird_baths %>%
  filter(!is.na(urban_rural)) %>%
  count(urban_rural, bioregions, bioreg_short, wt = bird_count) %>%
  mutate(n = pmin(n, 500)) %>%
  mutate(bioregions = stringr::str_wrap(bioregions, 10)) %>%
  tidyr::pivot_wider(id_cols = c(urban_rural), names_from = bioregions,
                     values_from = n) %>%
  ggradar(grid.max = 500, grid.mid = 250, values.radar = c("0", "250", ">500"),
          plot.legend = TRUE, plot.title = "Total bird count", axis.label.size = 4,
          group.colours = c("darkblue", "darkorange")) +
  theme(legend.position = c(0.91, 0.99))

urban <- lapply(urban3, function(x) {
  bird_baths %>%
    filter(!is.na(urban_rural), bird_type == x) %>%
    count(urban_rural, bioreg_short, bird_type, wt = bird_count) %>%
    mutate(n = pmin(n, 10)) %>%
    tidyr::pivot_wider(id_cols = c(urban_rural), names_from = bioreg_short,
                       values_from = n) %>%
    ggradar(grid.max = 10, grid.mid = 5, values.radar = c("0", "5", ">10"),
            plot.legend = FALSE, plot.title = x, axis.label.size = 4, group.point.size = 4,
            group.colours = c("darkblue", "darkorange"))
})

rural <- lapply(rural3, function(x) {
  bird_baths %>%
    filter(!is.na(urban_rural), bird_type == x) %>%
    count(urban_rural, bioreg_short, bird_type, wt = bird_count) %>%
    mutate(n = pmin(n, 10)) %>%
    tidyr::pivot_wider(id_cols = c(urban_rural), names_from = bioreg_short,
                       values_from = n) %>%
    ggradar(grid.max = 10, grid.mid = 5, values.radar = c("0", "5", ">10"),
            plot.legend = FALSE, plot.title = x, axis.label.size = 4, group.point.size = 4,
            group.colours = c("darkblue", "darkorange"))
})

layout_str <-
  "AABCD
AAEFG"

total + urban[[3]] + urban[[2]] + urban[[1]] + rural[[3]] + rural[[2]] + rural[[1]] +
  plot_layout(design = layout_str) +
  plot_annotation(subtitle = "Selected counts of bird species with high difference in urban and rural appearences",
                  caption = "Data: Cleary et al. (2016)") &
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 20),
        panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "grey90"),
        legend.background = element_rect(fill = "grey90"),
        legend.key = element_rect(fill = "transparent"),
        text = element_text(colour = "grey10"))
