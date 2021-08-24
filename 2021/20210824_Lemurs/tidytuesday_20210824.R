# Lemurs
# TidyTuesday 2021 week 35
# Rebecca Stevick updated 8/24/2021

# Load libraries -----------------
library(tidyverse)


# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-08-24')
lemurs <- tuesdata$lemurs

# Analysis and plotting ----------
lemurs %>%



   # add those labels
   labs(
     caption = "data from Duke Lemur Center Data via Kaggle | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("Lemurs_plot.png", width = 10, height = 6, dpi = 400)
