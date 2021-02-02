# HBCU enrollment
# TidyTuesday 2021 week 6
# Rebecca Stevick updated 2/2/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-02-02')

# Analysis and plotting ----------
 %>%


  # add those labels
  labs(caption = "data from Data.World | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("HBCUenrollment_plot.png", bg = "transparent", width = 15, height = 8, dpi = 400)
