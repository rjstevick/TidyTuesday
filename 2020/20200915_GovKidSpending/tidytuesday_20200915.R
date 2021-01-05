# Government spending on kids
# TidyTuesday 2020 week 38
# Rebecca Stevick updated 9/16/2020

# Load libraries ---------------
library(tidyverse)
library(gganimate)

# Load data --------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-15')
kids <- tuesdata$kids

# Analysis and plotting ------
basemap <- kids %>% 
  # select only PK12ed and join with the map data per state
  mutate(region=tolower(state)) %>% filter(variable=="PK12ed") %>% 
  left_join(map_data("state")) %>% 
  # make map area
  ggplot(aes(x = long, y = lat, group = group, fill = inf_adj_perchild)) + 
  geom_polygon(color = "white") +
  # change color scheme
  scale_fill_viridis_b(name = "Amount per child in $USD \n(adjusted for inflation)",
                       labels = c("$0"," ","$4"," ","$8"," ","$12"," ","$16"," ","$20"), # not the most elegant method, but it works...
                       breaks = seq(0, 20, 2), limits = c(0,20)) +
  # remove all theme elements and fix the x-y so the map doesn't warp
  theme_void() + coord_fixed(1.3) + 
  guides(fill = guide_colourbar(title.position="top")) +
  theme(plot.background = element_rect(fill="black"),plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.position = c(.18,.1), legend.direction = "horizontal", legend.key.width = unit(1, "cm"),
        text = element_text(family="Tahoma", color="white"), legend.title = element_text(size=9),
        plot.title = element_text(family = "Education Pencil", size = 15)) # font from https://fr.ffonts.net/Education-Pencil.font.download

#animate based on year
plotanimate<-basemap + transition_manual(frames = year) +
  # add those labels
  labs(caption = "data from Urban Institute | plot by @rjstevick for #TidyTuesday",
       title = "Education spending per child in the US has increased since 1996",
       subtitle = "Amount of money spent on PreK-12 education per child in {current_frame}")

# render animation
animate(plot = plotanimate, nframes = length(unique(kids$year)), 
        fps = 3, end_pause = 10, height = 450, width = 670, res = 100)

# Saving -----------------------
anim_save("GovKidSpending_plot.gif")
