# Makeup Shades
# TidyTuesday 2021 week 14
# Rebecca Stevick updated 3/30/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-03-30')
allShades <- tuesdata$allShades

# Analysis and plotting ----------


  # add those labels
  labs(

       caption = "data from The Pudding  |  plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("MakeupShades_plot.png", bg = "transparent", width = 14, height = 8, dpi = 400)
