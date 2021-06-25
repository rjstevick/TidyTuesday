# Public Park Access challenge tweets
# TidyTuesday 2021 week 26
# Rebecca Stevick updated 6/23/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-06-22')
parks <- tuesdata$parks

# Analysis and plotting ----------
parks %>%


   # add those labels
   labs(
        caption = "data from The Trust for Public Land | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("PublicParkAccess_plot.png", bg = "transparent", width = 12, height = 7, dpi = 400)
