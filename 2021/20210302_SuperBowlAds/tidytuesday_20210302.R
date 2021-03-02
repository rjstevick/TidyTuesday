# Superb Owl Ads
# TidyTuesday 2021 week 10
# Rebecca Stevick updated 3/2/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)
library(reshape2)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-03-02')
youtube <- tuesdata$youtube

# Analysis and plotting ----------
youtube %>%
  group_by(funny, show_product_quickly, patriotic, celebrity, danger, animals, use_sex) %>%
  count() %>% ungroup() %>% 
  mutate(funny = replace(funny, funny==TRUE, "funny"),
         show_product_quickly = replace(show_product_quickly, show_product_quickly==TRUE, "show_product_quickly"),
         patriotic = replace(patriotic, patriotic==TRUE, "patriotic"),
         celebrity = replace(celebrity, celebrity==TRUE, "celebrity"),
         danger = replace(danger, danger==TRUE, "danger"),
         animals = replace(animals, animals==TRUE, "animals"),
         use_sex = replace(use_sex, use_sex==TRUE, "use_sex")) %>% 
  na_if(FALSE) %>% 
#  add_column(row=1:64) %>% 
  unite("tactic", funny:use_sex, remove = FALSE) %>% 
  melt(id.vars=c("funny", "show_product_quickly", "patriotic", "celebrity", "danger", "animals", "use_sex")) -> hey
  
  
  
  pivot_longer(funny:use_sex) %>% 
  ggplot(aes(x=tactic, y=row, size=n, fill=n)) +
  geom_tile()
  
    ggplot(aes(x=name, y=value))

  # add those labels
  labs(
       caption = "data from FiveThirtyEight via superbowl-ads.com | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("SuperBowlAds_plot.png", bg = "transparent", width = 10, height = 6, dpi = 400)
