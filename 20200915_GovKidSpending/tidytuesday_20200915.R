# Government spending on kids
# TidyTuesday 2020 week 38
# Rebecca Stevick updated 9/15/2020

# Load libraries ---------------
library(tidyverse)

# Load data --------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-15')

# Analysis and plotting ------




  # add those labels
  labs(title="",
       caption="data from Urban Institute | plot by @rjstevick for #TidyTuesday")

# Saving -----------------------
ggsave("GovKidSpending_plot.png", width = 12, height = 6.5, dpi = 400)
