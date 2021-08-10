# BEA Infrastructure Investment
# TidyTuesday 2021 week 33
# Rebecca Stevick updated 8/10/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-08-10')
investment <- tuesdata$investment

# Analysis and plotting ----------
investment %>%


   # add those labels
   labs(
        caption = "data from Bureau of Economic Analysis | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("BEAInfrastructure_plot.png", bg = "transparent", width = 12, height = 7, dpi = 400)
