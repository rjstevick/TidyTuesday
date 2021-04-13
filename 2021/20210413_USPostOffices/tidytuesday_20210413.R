# US Post offices
# TidyTuesday 2021 week 16
# Rebecca Stevick updated 4/13/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-04-13')
post_offices <- tuesdata$post_offices

# Analysis and plotting ----------
post_offices %>%

   labs(
        caption = "data from Cameron Blevins and Richard W. Helbock  |  plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("USPostOffices_plot.png", bg = "transparent", width = 10, height = 6, dpi = 400)
