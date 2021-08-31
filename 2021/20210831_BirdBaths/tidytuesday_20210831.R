# Lemurs
# TidyTuesday 2021 week 36
# Rebecca Stevick updated 8/31/2021

# Load libraries -----------------
library(tidyverse)


# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-08-31')
bird_baths <- tuesdata$bird_baths

# Analysis and plotting ----------
bird_baths %>%



   # add those labels
   labs(
     caption = "data from Cleary et al. 2016  |  plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("BirdBaths_plot.png", width = 10, height = 6, dpi = 400)
