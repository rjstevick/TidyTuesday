# Ask a Manager Salary Survey
# TidyTuesday 2021 week 21
# Rebecca Stevick updated 5/18/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-05-18')
survey <- tuesdata$survey

# Analysis and plotting ----------
survey %>%


   # add those labels
   labs(title = "",
        caption = "data from Ask a Manager Salary Survey | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("SalarySurvey_plot.png", bg = "transparent", width = 8, height = 8, dpi = 400)
