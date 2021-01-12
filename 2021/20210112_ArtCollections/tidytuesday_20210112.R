# Art Collections
# TidyTuesday 2021 week 3
# Rebecca Stevick updated 1/12/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-01-12')

# Analysis and plotting ----------
tuesdata$artwork %>%

  # add those labels
  labs(
       caption = "data from Tate Art Museum | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("ArtCollections_plot.png", bg = "transparent", width = 12, height = 5, dpi = 400)
