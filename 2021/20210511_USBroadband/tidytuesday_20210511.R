# US broadband access
# TidyTuesday 2021 week 20
# Rebecca Stevick updated 5/11/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-05-11')
broadband <- tuesdata$broadband

# Analysis and plotting ----------
broadband %>%


   # add those labels
   labs(title = "",
        caption = "data from Microsoft GitHub | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("USBroadband_plot.png", bg = "transparent", width = 8, height = 8, dpi = 400)
