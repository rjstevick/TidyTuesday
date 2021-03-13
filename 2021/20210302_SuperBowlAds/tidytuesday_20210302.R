# Superb Owl Ads
# TidyTuesday 2021 week 10
# Rebecca Stevick updated 3/13/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-03-02')
youtube <- tuesdata$youtube

# Analysis and plotting ----------
youtube %>%   
  # put themes into one column
  pivot_longer(funny:use_sex) %>%
  # select only true rows
  filter(value==TRUE) %>% 
  # count number of ads per theme per year
  group_by(name, year) %>% count() %>% 
  # Fix theme names
  mutate(name = str_to_sentence(name), 
         name = recode(name, "Show_product_quickly"="Shows product quickly", "Use_sex"="Uses sex")) %>% 
  # start plotting
  ggplot(aes(x = year, y = name, fill = n)) +
  # add tiles and change color scheme
  geom_tile() + scale_fill_viridis_c(option = "E") +
  # change global theme
  theme_ipsum() +
  # edit theme
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        legend.title = element_text(vjust=0.75, family = "Copperplate"),
        plot.title = element_text(hjust=0.5, size=24, family = "Copperplate"), legend.position = "top") +
  # add those labels
  labs(title = "Superb Owl ad themes from XXXIV to LIV",
       x = NULL, y = NULL, fill = "Number of ads",
       caption = "data from FiveThirtyEight via superbowl-ads.com | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("SuperBowlAds_plot.png", bg = "transparent", width = 10, height = 6, dpi = 400)
