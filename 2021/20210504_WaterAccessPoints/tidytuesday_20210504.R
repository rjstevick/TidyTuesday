# Water access points
# TidyTuesday 2021 week 19
# Rebecca Stevick updated 5/6/2021

# Load libraries -----------------
library(tidyverse)
library(spData)
library(sf)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-05-04')
water <- tuesdata$water

# Analysis and plotting ----------
water %>%
   filter(lon_deg>=-20 & lon_deg<=50 & lat_deg>=-30 & lat_deg<=40) %>%
   replace_na(list(water_source="Unknown")) %>%
   mutate(water_source = case_when(
      str_detect(water_source, "Spring") ~ "Spring",
      str_detect(water_source, "Shallow Well") ~ "Shallow Well",
      str_detect(water_source, "Surface Water") ~ "Surface Water",
      TRUE ~ water_source)) %>%
   ggplot()+
   geom_sf(data = world %>% filter(continent == "Africa", !is.na(iso_a2)),
           aes(geometry=geom), fill = "grey90", color="white")+
   geom_hex(aes(x = lon_deg, y = lat_deg, fill = water_source),
            bins = 80, alpha = 0.6)+
   theme_void() +
   scale_fill_brewer(palette = "Set3") +
   theme(legend.position = c(0.15, 0.25), title = element_text(family = "Baloo"),
         plot.title = element_text(size = 26))+
   # add those labels
   labs(title = "Water sources in Africa installed since 1900", fill = "Water Source",
        caption = "data from Water Point Data Exchange | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("WaterAccessPoints_plot.png", bg = "transparent", width = 8, height = 8, dpi = 400)
