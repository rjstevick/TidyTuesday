# Paralympic Medals
# TidyTuesday 2021 week 32
# Rebecca Stevick updated 8/3/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-08-03')
athletes <- tuesdata$athletes

# Analysis and plotting ----------
athletes %>%


   # add those labels
   labs(
        caption = "data from International Paralympic Committee | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("ParalympicMedals_plot.png", bg = "transparent", width = 12, height = 7, dpi = 400)
