# Ninja Warrior
# TidyTuesday 2020 week 51
# Rebecca Stevick updated 12/15/2020

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-12-15')

# Analysis and plotting ----------
tuesdata$ninja_warrior %>%

  # add those labels
  labs(
       caption = "data from the Data.World | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("NinjaWarrior_plot.png", bg = "transparent", width = 10, height = 5, dpi = 400)
