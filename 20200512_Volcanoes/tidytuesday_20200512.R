# Volcano Eruptions!
# TidyTuesday 2020 week 20
# RJS updated 5/12/2020

# Load libraries ---------------------

library(tidyverse)
library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)

sessionInfo()
theme_set(theme_light())

# Load data --------------------------

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

# world map
w2hr <- map_data("world2")

# Data formatting & analysis ---------



# Plotting ---------------------------

# old map plot code
ggplot() +
  geom_polygon(data=w2limit, aes(x=long, y=lat, group=group), fill="grey30", color="grey60") +
  coord_fixed(1.3) + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))+
  geom_point(data, mapping = aes(x = absLong, y = Lat, fill = StateCountry), size=5, shape=23)+
  scale_fill_manual(values=c("#42c2bf",
                             "#e666a8",
                             "#46c381",
                             "#c080da",
                             "#72b155",
                             "#7087ed",
                             "#b1aa39",
                             "#519bda",
                             "#d5904c",
                             "#55b9e4",
                             "#e3766e",
                             "#5aab84",
                             "#d387b4",
                             "#a4a660",
                             "#9797da"))+
  theme_nothing() + theme(legend.direction="horizontal",
                          #legend.position =  c(0.43,0.4),
                          legend.position="bottom",
                          legend.title = element_blank(), legend.text = element_text(color="grey30", size=16),
                          panel.background = element_rect(fill = "white", color="white"))






ggplot( , aes()) +
  labs(
    title = "",
    subtitle = "",
    caption = "Plot by @rjstevick \n Source: XXXXXXXXXX",
    x = "", y = ""
    ) +
  theme(plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 0.5),
        text = element_text(size=18),
        legend.position = "none")


# Saving -----------------------------

ggsave("Volcanoes_plot.png", bg="transparent", width = 10.5, height = 6.5, dpi=400)
