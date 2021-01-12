# Art Collections
# TidyTuesday 2021 week 3
# Rebecca Stevick updated 1/12/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-01-12')
artwork <- tuesdata$artwork
artists <- tuesdata$artists

# Analysis and plotting ----------
tuesdata$artwork %>%
  group_by(year) %>% summarise(meanheight=mean(height), meanwidth=mean(width)) %>% 
  ggplot()+
  geom_rect(aes(xmin=0, xmax=meanwidth, ymin=0, ymax=meanheight, fill=year), 
            color="white", alpha=0.6)+
  theme_minimal()

tuesdata$artwork %>%
  group_by(year) %>% summarise(meanheight=mean(height), meanwidth=mean(width)) %>% 
  mutate(meanarea=meanheight*meanwidth) %>% 
  ggplot(aes(x=year, y=meanarea))+
  geom_line()+
  theme_minimal()
  
  # add those labels
  labs(
       caption = "data from Tate Art Museum | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("ArtCollections_plot.png", bg = "transparent", width = 12, height = 5, dpi = 400)
