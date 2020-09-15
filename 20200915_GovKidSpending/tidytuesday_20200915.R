# Government spending on kids
# TidyTuesday 2020 week 38
# Rebecca Stevick updated 9/15/2020

# Load libraries ---------------
library(tidyverse)
library(gganimate)


# Load data --------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-15')
kids <- tuesdata$kids

# Analysis and plotting ------

basemap <- kids %>% 
  mutate(region=tolower(state)) %>% 
  filter(variable=="PK12ed") %>% 
  left_join(map_data("state")) %>% 
  # make map area
  ggplot(aes(x = long, y = lat, group = group, fill = inf_adj_perchild)) + 
  # add world map
  geom_polygon(color = "white") +
  # change color scheme
  scale_fill_viridis_c(name = "Amount per child in $USD \n(adjusted for inflation)",
                       labels = scales::dollar_format()) +
  # remove all theme elements and fix the x-y so the map doesn't warp
  theme_void() + coord_fixed(1.3) + theme(legend.position = "bottom")

#animate based on year
plotanimate<-basemap + transition_manual(frames = year) +
  # add those labels
  labs(caption="data from Urban Institute | plot by @rjstevick for #TidyTuesday",
       title="Education spending per child in the US has increased since 1996",
       subtitle="Amount of money spent on PreK-12 education per child in {current_frame}")

# render animation
animate(plot = plotanimate, nframes = length(unique(kids$year)), 
        fps = 4, end_pause = 8, height = 450, width = 740, res = 100)

# Saving -----------------------
anim_save("GovKidSpending_plot.gif")
