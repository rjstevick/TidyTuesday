# Billboard Top 100
# TidyTuesday 2021 week 38
# Rebecca Stevick updated 9/16/2021

# Load libraries -----------------
library(tidyverse)


# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-09-14')
billboard <- tuesdata$billboard

# Analysis and plotting ----------
billboard %>%



   # add those labels
   labs(
     caption = "data from Data.World via Billboard.com & Spotify |  plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("BillboardTop100_plot.png", width = 10, height = 6, dpi = 400)
