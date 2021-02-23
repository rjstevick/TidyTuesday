# Du Bois Challenge
# TidyTuesday 2021 week 8
# Rebecca Stevick updated 2/16/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
# Data for plate 2: Relative Negro Population of the States of the United States
tuesdata <- read_csv('https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/plate02/data.csv')

# function to increase vertical spacing between legend keys by @clauswilke.
# https://stackoverflow.com/questions/11366964/is-there-a-way-to-change-the-spacing-between-legend-items-in-ggplot2
draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  grid::rectGrob(width = grid::unit(0.6, "npc"), height = grid::unit(0.6, "npc"),
                 gp = grid::gpar(col = data$colour, fill = alpha(data$fill, data$alpha), lty = data$linetype, lwd = lwd * .pt, linejoin = "mitre"))}

# Analysis and plotting ----------
tuesdata %>%
  # make a dataframe of state names and abbreviations, join with population data
  left_join(data.frame(state.abb, state.name), by = c("State" = "state.abb")) %>%
  # make state names lowercase and order the population category
  mutate(region = tolower(state.name),
         Population = factor(Population, levels = c("750,000 AND OVER", "600,000 - 750,000", "500,000 - 600,000", "300,000 - 500,000", "200,000 - 300,000",
                                                    "100,000 - 200,000", "50,000 - 100,000", "25,000 - 50,000", "10,000 - 25,000", "UNDER - 10,000"))) %>%
  # join data with map data
  left_join(map_data("state")) %>%
  # make map area, fill by population
  ggplot(aes(x = long, y = lat, group = group, fill = Population)) +
  # add states, with thinner grey outlines and use spread out legend keys (polygon3)
  geom_polygon(color = "grey20", lwd = 0.2, key_glyph = "polygon3") +
  # remove all theme elements and fix the x-y so the map doesn't warp
  theme_void() + coord_fixed(1.3) +
  # define fill colors
  scale_fill_manual(values = c("#20211c", "#9a8d7d", "seashell3", "#6f543e", "#2c2449", "#bd354d", "#d5afa6", "#dcac41", "#d2c5b2")) +
  # edit the theme
  theme(text = element_text(family = "Charter"), plot.title = element_text(margin = margin(t=10, b = 100), size = 18, hjust = 0.5, face = "bold"),
        plot.caption = element_text(size=8, hjust=0.5, margin = margin(t = 30, unit = "pt")),
        plot.margin = margin(r = 50, l = 50, unit = "pt"),
        legend.position = "bottom", legend.text = element_text(color = "grey20", margin = margin(r = 40, unit = "pt")))+
  # put legend items in 2 columns
  guides(fill = guide_legend(ncol = 2,  keyheight = unit(12, units = "mm"), keywidth = unit(12, units = "mm"))) +
  # add those labels
  labs(title = "RELATIVE NEGRO POPULATION OF THE STATES OF THE \nUNITED STATES.", fill = NULL,
       caption = "recreation of W.E.B Du Bois's Plate 2: Relative Negro Population of the States of the United States
       data from Du Bois data challenge | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("DuBoisChallenge_plot.png", bg = "#dfd3c3", width = 7, height = 8.5, dpi = 400)
