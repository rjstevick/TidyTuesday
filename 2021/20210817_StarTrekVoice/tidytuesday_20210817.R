# Star Trek Voice Commands
# TidyTuesday 2021 week 34
# Rebecca Stevick updated 8/17/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-08-17')
computer <- tuesdata$computer

# Analysis and plotting ----------
computer %>%


   # add those labels
   labs(
        caption = "data from SpeechInteraction.org | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("StarTrekVoice_plot.png", bg = "transparent", width = 12, height = 7, dpi = 400)
