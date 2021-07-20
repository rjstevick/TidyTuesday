# US Scooby Doo
# TidyTuesday 2021 week 29
# Rebecca Stevick updated 7/20/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-07-13')
scoobydoo <- tuesdata$scoobydoo

# Analysis and plotting ----------
scoobydoo %>%

   # add those labels
   labs(
        caption = "data from Kaggle | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("ScoobyDoo_plot.png", bg = "transparent", width = 12, height = 7, dpi = 400)
