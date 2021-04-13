# US Post offices
# TidyTuesday 2021 week 16
# Rebecca Stevick updated 4/13/2021

# Load libraries -----------------
library(tidyverse)
library(gganimate)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-04-13')
post_offices <- tuesdata$post_offices
post_offices_clean <- post_offices %>% 
  filter(established<=1850) %>% 
  filter(longitude <= 0) 

# Analysis and plotting ----------
plot <- post_offices_clean %>% 
  ggplot(aes(y=latitude, x=longitude)) +
  geom_point(size=0.6, shape=19, alpha=0.2) +
  theme_void() +
  coord_fixed(1.3) +
  labs(title = "Post offices",
       caption = "data from Cameron Blevins and Richard W. Helbock  |  plot by @rjstevick for #TidyTuesday")

plotanimate <- plot + 
  transition_manual(frames = established, cumulative = TRUE)+
  labs(subtitle="Cumulative volcano eruptions per year: {current_frame}")

animate(plot = plotanimate, 
        nframes = length(unique(post_offices_clean$established)), 
        fps = 20, height = 500, width = 1000, res = 100)



# Saving -------------------------
ggsave("USPostOffices_plot.png", bg = "transparent", width = 10, height = 6, dpi = 400)
