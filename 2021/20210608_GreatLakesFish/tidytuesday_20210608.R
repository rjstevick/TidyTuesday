# Great Lakes Fish
# TidyTuesday 2021 week 24
# Rebecca Stevick updated 6/8/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-06-08')
fishing <- tuesdata$fishing
stocked <- tuesdata$stocked

# Analysis and plotting ----------
fishing %>%


   # add those labels
   labs(
        caption = "data from Great Lakes Fishery Commission | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("GreatLakesFish_plot.png", bg = "transparent", width = 12, height = 7, dpi = 400)
