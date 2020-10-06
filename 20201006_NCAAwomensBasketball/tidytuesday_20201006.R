# NCAA Women's Basketball
# TidyTuesday 2020 week 41
# Rebecca Stevick updated 10/6/2020

# Load libraries ---------------
library(tidyverse)

extrafont::loadfonts()

# Load data --------------------


# Analysis and plotting ------



  # add those labels
  labs(y="", x=NULL,
       title="",
       caption="data from FiveThirtyEight | plot by @rjstevick for #TidyTuesday")

# Saving -----------------------
ggsave("NCAAwomensBasketball_plot.png", bg="transparent", width = 7, height = 5, dpi = 400)
