# Canadian Wind Turbines
# TidyTuesday 2020 week 44
# Rebecca Stevick updated 11/1/2020

# Load libraries -----------------
library(tidyverse)
library(ggrepel)
library(extrafont)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-10-27')

# Analysis and plotting ----------
tuesdata$`wind-turbine` %>% 
  # group the data by province/territory to calculate summary statistics
  group_by(province_territory) %>%
  # now summarize the data for count and mean of variables
  summarise(count=n(), # count the number of windmills in each province
            meandiam=mean(rotor_diameter_m), # mean of their diameters
            meanlat=mean(latitude), # mean of their latitudes
            meanlong=mean(longitude)) %>% # mean of their longitudes
  # start to plot
  ggplot()+
  # add a map of Canada
  geom_polygon(data=map_data("world") %>% filter(region=="Canada"), 
               aes(x=long, y=lat, group=group), fill="grey90", color="white") +
  # add a point and star at each mean windmill location
  geom_point(aes(x=meanlong, y=meanlat, color=count), size=4, shape=20)+
  geom_point(aes(x=meanlong, y=meanlat, size=meandiam, color=count), shape=8)+
  # add labels for the points
  geom_text_repel(aes(x=meanlong, y=meanlat, color=count, 
                      label=str_wrap(province_territory, 14)), # wrap the labels so they fit better
                      nudge_y=1.5, family="Amaranth", segment.alpha=0)+
  # change the color scheme
  scale_color_gradient(low="red3",high="black")+
  # add overall theme and fix the coordinates so the map doesn't warp
  theme_void() + coord_fixed(1.3)+
  # edit the theme and legend position
  theme(text=element_text(family="Amaranth"),
        legend.position=c(0.9,0.7), legend.box="horizontal", legend.direction = "vertical")+
  # add title to the top left of the plot
  geom_text(aes(x=-110, y=78, label="Canadian Windmills by Province/Territory"),
            size=7, fontface="bold", family="Amaranth")+
  # add subtitle under the title
  geom_text(aes(x=-110, y=76, label="Each point represents the mean location, size, and diameter of the windmills in the territory."),
            size=4, family="Amaranth", color="red4")+
  # add caption on bottom left inset
  geom_text(aes(x=-125, y=43, label="data from open.canada.ca | plot by @rjstevick for #TidyTuesday"),
            size=3, family="Amaranth", color="grey60")+
  # add those labels
  labs(color="Number of \nWindmills", 
       size="Mean diameter \nof Windmills (m)")

# Saving -------------------------
ggsave("CanadianWindTurbines_plot.png", bg="transparent", width = 11, height = 7, dpi = 400)
