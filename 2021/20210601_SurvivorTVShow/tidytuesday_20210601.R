# Survivor TV Show
# TidyTuesday 2021 week 23
# Rebecca Stevick updated 6/25/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-06-01')
summary <- tuesdata$summary

# Analysis and plotting ----------
summary %>%
   # count seasons per country
   group_by(country) %>% count(sort = TRUE) %>% 
   # join with map data
   left_join(map_data("world"), by=c("country" = "region")) %>% 
   # start plotting
   ggplot() +
   # add background map
   geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill="grey90") +
   # add filled country counts
   geom_polygon(aes(x = long, y = lat, group = group, fill = n), color = "white", lwd=0.5) +
   # highlight Fiji with star, arrow, and label
   geom_point(aes(x = 175, y = -21), shape = 8, color = "darkgoldenrod1") +
   geom_text(aes(x = 170, y = 8, label = "Fiji hosted the \nmost seasons (9)"),
             colour = "darkgoldenrod2", family = "Roboto Condensed", hjust = 0.5, vjust = 0, size = 3) +
   geom_curve(aes(x = 170, y = 5, xend = 175, yend = -19), colour = "darkgoldenrod2", size = 0.5, 
              curvature = -0.2, arrow = arrow(length = unit(0.02, "npc"))) +
   # edit fill color scheme
   scale_fill_viridis_c(option = "magma", breaks = scales::breaks_width(2)) +
   # set global theme and fix coordinates
   theme_void() + coord_fixed(1.3) +
   # edit theme
   theme(plot.title = element_text(family = "Survivant", size = 20, hjust = 0.5, color = "purple4"),
         text = element_text(family = "Roboto Condensed"),
         legend.position = c(0.15,0.2), legend.direction = "horizontal") +
   # put legend label on top
   guides(fill = guide_colourbar(title.position="top")) +
   # add those labels
   labs(title = "Survivor Filming locations",
        fill = "Number of seasons",
        caption = "data from survivorR R package via Daniel Oehm | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("SurvivorTVShow_plot.png", bg = "transparent", width = 8, height = 5.6, dpi = 400)
