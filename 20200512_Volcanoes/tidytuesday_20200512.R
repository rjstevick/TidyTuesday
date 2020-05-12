# Volcano Eruptions!
# TidyTuesday 2020 week 20
# RJS updated 5/12/2020

# Load libraries ---------------------

library(tidyverse)
library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)
library(gganimate)

sessionInfo()
theme_set(theme_light())

# Load data --------------------------

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
#events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
#tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
#sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

w2hr <- map_data("world") # world map

# Data formatting & analysis ---------

volcanoerupt<-left_join(volcano, eruptions) %>%
  drop_na(c(start_year,vei)) %>%
  filter(start_year>1960, start_year<2017)

# Plotting ---------------------------

plotv<- ggplot() +
  geom_polygon(data=w2hr, 
               aes(x=long, y=lat, group=group), 
               fill="grey30", color="grey60") +
  geom_point(volcanoerupt, 
             mapping = aes(x = longitude, y = latitude,
                           color = elevation, size=vei,
                           # add a group so the points don't animate from each other
                           group=as.factor(start_year)), 
             shape=17, alpha=0.6)+
  scale_colour_viridis_c("Elevation (m)", option="magma")+
  scale_size_continuous("Volcano \nExplosivity \nIndex")+
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
  coord_fixed(1.3) + theme_minimal() + 
  labs(caption = "Plot by @rjstevick \n Data since 1960, Source: Smithsonian & Wikipedia", 
       x = NULL, y = NULL) +
  theme(legend.text = element_text(color="grey30", size=14),
        axis.text = element_blank())

#animate based on year volcano started erupting
plotanimate<-plotv+
  transition_manual(frames=start_year, cumulative = TRUE)+
  ggtitle("Cumulative volcano eruptions per year: {current_frame}")

# render animation
animate(plot = plotanimate, 
  nframes = length(unique(volcanoerupt$start_year)), 
  fps = 4, end_pause = 8, height = 380, width =600)

## Saving -----------------------------

anim_save("Volcanoes_plot.gif")
