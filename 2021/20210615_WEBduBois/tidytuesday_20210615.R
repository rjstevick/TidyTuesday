# WEB Du Bois challenge tweets
# TidyTuesday 2021 week 25
# Rebecca Stevick updated 6/23/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-06-15')
tweets <- tuesdata$tweets

# Analysis and plotting ----------
tweets %>%


   # add those labels
   labs(
        caption = "data from `#DuBoisChallenge` tweets | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("WEBduBois_plot.png", bg = "transparent", width = 12, height = 7, dpi = 400)
