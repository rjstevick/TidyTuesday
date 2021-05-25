# Mario Kart World Records
# TidyTuesday 2021 week 22
# Rebecca Stevick updated 5/25/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-05-25')
records <- tuesdata$records
drivers <- tuesdata$drivers

# Analysis and plotting ----------
records %>%


   # add those labels
   labs(title = "",
        caption = "data from Mario Kart World Records | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("MarioKart_plot.png", bg = "transparent", width = 8, height = 8, dpi = 400)
