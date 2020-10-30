# Canadian Wind Turbines
# TidyTuesday 2020 week 44
# Rebecca Stevick updated 10/27/2020

# Load libraries -----------------
library(tidyverse)
library(ggrepel)
library(extrafont)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-10-27')

# Analysis and plotting ----------

tuesdata$`wind-turbine` %>% 
  group_by(province_territory) %>% 
  summarise(count=n(), 
            meanrad=mean(rotor_diameter_m), 
            meanlat=mean(latitude), 
            meanlong=mean(longitude)) %>% 
  ggplot()+
  geom_polygon(data=map_data("world") %>% filter(region=="Canada"), 
               aes(x=long, y=lat, group=group), fill="grey90", color="white") +
  geom_point(aes(x=meanlong, y=meanlat, color=count), size=4, shape=20)+
  geom_point(aes(x=meanlong, y=meanlat, size=meanrad, color=count), shape=8)+
  geom_text_repel(aes(x=meanlong, y=meanlat, color=count, label=str_wrap(province_territory, 10)),
            nudge_y=1.5, family="Amaranth", segment.alpha=0)+
  geom_text(aes(x=-110, y=78, label="Canadian Windmills by Province/Territory"),
            size=7, fontface="bold", family="Amaranth")+
  geom_text(aes(x=-110, y=76, 
                label="Each point represents the mean location, size, and radius of the windmills in the territory."),
            size=4, family="Amaranth", color="red3")+
  scale_color_gradient(low="red3",high="black")+
  theme_void() + coord_fixed(1.3)+
  theme(text=element_text(family="Amaranth"),
        legend.position=c(0.85,0.8), legend.box="horizontal", legend.direction = "vertical")+
  # add those labels
  labs(color="Number of \nWindmills", size="Mean radius \nof Windmills",
       caption="data from open.canada.ca | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("CanadianWindTurbines_plot.png", bg="transparent", width = 11, height = 7, dpi = 400)
