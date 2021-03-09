# Bechdel Test
# TidyTuesday 2021 week 11
# Rebecca Stevick updated 3/10/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-03-09')
bechdel <- tuesdata$bechdel
movies <- tuesdata$movies


# Analysis and plotting ----------

  # add those labels
  labs(
       caption = "data from FiveThirtyEight | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("BechdelTest_plot.png", bg = "transparent", width = 10, height = 6, dpi = 400)
