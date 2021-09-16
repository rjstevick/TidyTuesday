# Formula 1 Races
# TidyTuesday 2021 week 37
# Rebecca Stevick updated 9/16/2021

# Load libraries -----------------
library(tidyverse)


# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-09-07')
circuits <- tuesdata$circuits
races <- tuesdata$races
results <- tuesdata$results
drivers <- tuesdata$drivers

# Analysis and plotting ----------
circuits %>%



   # add those labels
   labs(
     caption = "data from Ergast API |  plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("Formula1Races_plot.png", width = 10, height = 6, dpi = 400)
