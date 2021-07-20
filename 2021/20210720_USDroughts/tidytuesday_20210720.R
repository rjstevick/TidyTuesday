# US droughts
# TidyTuesday 2021 week 30
# Rebecca Stevick updated 7/20/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-07-20')
drought <- tuesdata$drought

# Analysis and plotting ----------
drought %>%


   # add those labels
   labs(
        caption = "data from U.S. Drought Monitor | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("USDroughts_plot.png", bg = "transparent", width = 12, height = 7, dpi = 400)
