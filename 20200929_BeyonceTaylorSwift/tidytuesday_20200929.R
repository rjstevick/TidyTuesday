# Beyonce & Taylor Swift Lyrics
# TidyTuesday 2020 week 40
# Rebecca Stevick updated 9/29/2020

# Load libraries ---------------
library(tidyverse)
library(hrbrthemes)

# Load data --------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-29')
beyonce_lyrics <- tuesdata$beyonce_lyrics


# Analysis and plotting ------




  # add those labels
  labs(x=NULL, y=NULL,
       title="Nepalese climbers have participated in the most first ascents of the Himalayas",
       subtitle="Timeline of first ascents of Himalayan Peaks by country",
       caption="All countries with more than 2 first ascents are shown.
       data from The Himalayan Database/Elizabeth Hawley | plot by @rjstevick for #TidyTuesday")

# Saving -----------------------
ggsave("BeyonceTaylorSwift_plot.png", bg="transparent", width = 12, height = 6.5, dpi = 400)
