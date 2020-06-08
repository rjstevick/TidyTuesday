# African American Achievements
# TidyTuesday 2020 week 24
# Rebecca Stevick updated 6/9/2020

# Load libraries
library(tidyverse)

# Load data
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')




# Saving -----------------------------
ggsave("AfricanAmericanAchievements_plot.png", bg="transparent", width = 12, height = 6.5, dpi=400)
