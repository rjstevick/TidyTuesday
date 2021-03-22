# UN Votes
# TidyTuesday 2021 week 13
# Rebecca Stevick updated 3/23/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-03-23')
unvotes <- tuesdata$unvotes
issues <- tuesdata$issues


# Analysis and plotting ----------
issues %>%

   # add those labels
   labs(
        caption = "data from Harvard Dataverse  |  plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("UNVotes_plot.png", width = 9.5, height = 6, dpi = 400)
