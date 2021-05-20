# US broadband access
# TidyTuesday 2021 week 20
# Rebecca Stevick updated 5/20/2021

# Load libraries -----------------
library(tidyverse)
library(geofacet)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-05-11')
broadband <- tuesdata$broadband

# Analysis and plotting ----------
broadband %>%
   drop_na(`BROADBAND AVAILABILITY PER FCC`) %>% 
   mutate(availability = as.numeric(`BROADBAND AVAILABILITY PER FCC`)) %>% 
   # time to plot
   ggplot(aes(x=1, y=1)) + 
   geom_jitter(aes(color = availability), alpha=0.8) +
   geom_text(data = broadband %>% distinct(ST), 
             aes(label = ST), size=8, family = "Copperplate")+
   # make a panel for each state in its geographical location (relatively)
   facet_geo(~ST) + 
   # edit color scale
   scale_color_viridis_c(option ="B", labels = scales::label_percent()) +
   # change global theme
   theme_void() +
   # edit theme
   theme(text=element_text(size=18, family = "Andale Mono"), 
         plot.title=element_text(face="bold", size=30), plot.subtitle = element_text(size = 14),
         strip.text = element_blank(), legend.direction = "horizontal",
         panel.background = element_rect(fill="snow2", color="transparent"),
         panel.spacing = unit(0.2, "lines"),  plot.margin = margin(10, 10, 10, 10),
         legend.position=c(0.2, 0.95), legend.text=element_text(size=16),
         legend.key.width = unit(1.3, "cm")) +
   guides(color = guide_colourbar(title.position="top")) +
   # add those labels
   labs(title = "Broadband Availability in the US",
        subtitle = "Each point represents a county, colored by percent of people per county with access to \nfixed terrestrial broadband at speeds of 25 Mbps/3 Mbps as of the end of 2017\n",
        color = NULL,
        caption = "\ndata from Microsoft GitHub \nplot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("USBroadband_plot.png", bg = "transparent", width = 12, height = 8, dpi = 400)
