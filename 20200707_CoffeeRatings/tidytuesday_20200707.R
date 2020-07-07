# Coffee Ratings
# TidyTuesday 2020 week 28
# Rebecca Stevick updated 7/7/2020

# Load libraries
library(tidyverse)
library(hrbrthemes)

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-07-07')
coffee_ratings <- tuesdata$coffee_ratings


# Saving -----------------------------
ggsave("CoffeeRatings_plot.png", width = 12, height = 6.5, dpi=400)
