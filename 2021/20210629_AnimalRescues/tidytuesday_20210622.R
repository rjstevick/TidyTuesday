# London Animal Rescues
# TidyTuesday 2021 week 27
# Rebecca Stevick updated 6/29/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-06-29')
animal_rescues <- tuesdata$animal_rescues

# Analysis and plotting ----------
animal_rescues %>%


   # add those labels
   labs(
        caption = "data from The London Fire Brigade via london.gov | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("AnimalRescues_plot.png", bg = "transparent", width = 12, height = 7, dpi = 400)
