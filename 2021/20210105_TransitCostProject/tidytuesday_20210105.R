# Transit cost project
# TidyTuesday 2021 week 2
# Rebecca Stevick updated 1/5/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-01-05')

# Analysis and plotting ----------
tuesdata$`big-mac` %>%

  # add those labels
  labs(
       caption = "data from the Transit Costs Project | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("TransitCostProject_plot.png", bg = "transparent", width = 12, height = 5, dpi = 400)
