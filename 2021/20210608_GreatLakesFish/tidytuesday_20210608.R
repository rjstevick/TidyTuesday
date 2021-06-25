# Great Lakes Fish
# TidyTuesday 2021 week 24
# Rebecca Stevick updated 6/25/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-06-08')
fishing <- tuesdata$fishing
stocked <- tuesdata$stocked

# Analysis and plotting ----------
fishing %>%
   # fix some names
   mutate(species = recode(species, 
                           "Amercian Eel" = "American Eel",
                           "White bass" = "White Bass",
                           "Bullheads" = "Bullhead",
                           "Channel catfish" = "Channel Catfish",
                           "Cisco and chubs" = "Cisco and Chubs",
                           "Cisco and Chub" = "Cisco and Chubs",
                           "Pacific salmon" = "Pacific Salmon",
                           "Crappies" = "Crappie")) %>% 
   # pick most abundant species
   group_by(species) %>% mutate(sum = sum(grand_total, na.rm = TRUE)) %>% 
   filter(sum > 5000000) %>% 
   # remove summary regions
   filter(!grepl("Total", region)) %>% 
   # remove absent data
   drop_na(grand_total, year) %>% 
   # start plotting
   ggplot(aes(x = year, y = grand_total, color = species)) +
   geom_point(size = 0.5) + 
   geom_smooth(alpha = 0.2) +
   facet_wrap(.~region) +
   scale_color_manual(values = PNWColors::pnw_palette("Cascades", n = 4)) +
   theme_ipsum() +
   theme(legend.position = "top", legend.justification = "left") +
   # add those labels
   labs(x = NULL, y = "Total observations", color = NULL, 
        title = "Time-series of 4 most abundant fish species in the Great Lakes",
        caption = "data from Great Lakes Fishery Commission | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("GreatLakesFish_plot.png", bg = "transparent", width = 12, height = 7, dpi = 400)
