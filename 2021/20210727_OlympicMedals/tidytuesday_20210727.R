# Olympic Medals
# TidyTuesday 2021 week 31
# Rebecca Stevick updated 7/27/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-07-27')
olympics <- tuesdata$olympics

# Analysis and plotting ----------
olympics %>%


   # add those labels
   labs(
        caption = "data from Kaggle | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("OlympicMedals_plot.png", bg = "transparent", width = 12, height = 7, dpi = 400)
