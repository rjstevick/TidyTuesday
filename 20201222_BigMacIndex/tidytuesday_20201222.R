# Ninja Warrior
# TidyTuesday 2020 week 52
# Rebecca Stevick updated 12/22/2020

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-12-22')

# Analysis and plotting ----------
big-mac <- tuesdata$big-mac

big-mac %>% 
  # add those labels
  labs(
       caption = "data from the The Economist | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("BigMacIndex_plot.png", bg = "transparent", width = 11, height = 5, dpi = 400)
