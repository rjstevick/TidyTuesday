# Water access points
# TidyTuesday 2021 week 19
# Rebecca Stevick updated 5/4/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-05-04')
water <- tuesdata$water

# Analysis and plotting ----------
water %>%


   # add those labels
   labs(title = "",
        caption = "data from Water Point Data Exchange | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("WaterAccessPoints_plot.png", bg = "transparent", width = 10, height = 6, dpi = 400)
