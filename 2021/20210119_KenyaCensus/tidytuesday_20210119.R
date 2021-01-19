# Kenya Census
# TidyTuesday 2021 week 3
# Rebecca Stevick updated 1/19/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-01-19')

# Analysis and plotting ----------
tuesdata$gender %>%

  # add those labels
  labs(
       caption = "data from rKenyaCensus | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("KenyaCensus_plot.png", bg = "transparent", width = 9, height = 5, dpi = 400)
