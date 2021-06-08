# Survivor TV Show
# TidyTuesday 2021 week 23
# Rebecca Stevick updated 6/8/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-06-01')
summary <- tuesdata$summary

# Analysis and plotting ----------
summary %>%


   # add those labels
   labs(
        caption = "data from survivorR R package via Daniel Oehm | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("SurvivorTVShow_plot.png", bg = "transparent", width = 12, height = 7, dpi = 400)
