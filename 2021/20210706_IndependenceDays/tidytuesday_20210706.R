# International Independence Days
# TidyTuesday 2021 week 28
# Rebecca Stevick updated 7/6/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-07-06')
holidays <- tuesdata$holidays

# Analysis and plotting ----------
holidays %>%


   # add those labels
   labs(
        caption = "data from Wikipedia| plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("IndependenceDays_plot.png", bg = "transparent", width = 12, height = 7, dpi = 400)
