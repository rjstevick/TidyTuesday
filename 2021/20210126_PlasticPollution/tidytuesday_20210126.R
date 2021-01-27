# Plastic Pollution
# TidyTuesday 2021 week 5
# Rebecca Stevick updated 1/26/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-01-26')

# Analysis and plotting ----------
plastics %>%
  filter(grepl("coca", parent_company, ignore.case = TRUE))


  # add those labels
  labs(caption = "data from `Break Free from Plastic` | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("PlasticPollution_plot.png", bg = "transparent", width = 15, height = 8, dpi = 400)
