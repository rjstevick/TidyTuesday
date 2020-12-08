# Women of 2020
# TidyTuesday 2020 week 50
# Rebecca Stevick updated 12/08/2020

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-12-08')

# Analysis and plotting ----------
tuesdata$women %>%

  # add those labels
  labs(
       caption = "data from the BBC | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("Women2020_plot.png", bg = "transparent", width = 10, height = 5, dpi = 400)
