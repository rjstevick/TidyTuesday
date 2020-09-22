# Himalayan Climbers
# TidyTuesday 2020 week 39
# Rebecca Stevick updated 9/22/2020

# Load libraries ---------------
library(tidyverse)

# Load data --------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-22')
climbers <- tuesdata$members
expeditions <- tuesdata$expeditions
peaks <- tuesdata$peaks

# Analysis and plotting ------




  # add those labels
  labs(caption = "data from The Himalayan Database/Elizabeth Hawley | plot by @rjstevick for #TidyTuesday",
       title = "",
       subtitle = "")


# Saving -----------------------
ggsave("HimalayanClimbers_plot.png", width = 12, height = 6.5, dpi = 400)
