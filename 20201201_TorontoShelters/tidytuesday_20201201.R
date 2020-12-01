# Toronto homeless shelters
# TidyTuesday 2020 week 49
# Rebecca Stevick updated 12/01/2020

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-12-01')

# Analysis and plotting ----------
tuesdata$shelters %>%
  group_by(shelter_postal_code) %>% 
  summarise(sumoccupancy=sum(occupancy),
            sumcapacity=sum(capacity, na.rm=TRUE),
            count=n()) %>% 
  filter(sumcapacity != 0) %>% 
  mutate(percentcapacity=sumoccupancy/sumcapacity) %>% 
  ggplot(aes(x=count, y=percentcapacity))+
  geom_point()

  # add those labels
  labs(
       caption = "data from opendatatoronto | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("TorontoShelters_plot.png", bg = "transparent", width = 14, height = 8, dpi = 400)
