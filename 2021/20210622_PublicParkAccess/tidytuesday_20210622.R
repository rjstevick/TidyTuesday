# Public Park Access challenge tweets
# TidyTuesday 2021 week 26
# Rebecca Stevick updated 8/4/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-06-22')
parks <- tuesdata$parks

# Analysis and plotting ----------
parks %>%
   filter(year == 2020) %>%
   filter(pct_near_park_points >75) %>%
   ggplot(aes(y=pct_near_park_points, x=reorder(str_wrap(city,10),pct_near_park_points), fill=pct_near_park_points))+
   geom_col(color = "white")+
   coord_polar(clip = "off")+
   scale_y_continuous(limits=c(0,100))+
   scale_fill_viridis_c(option="B", labels = scales::label_percent(scale=1, accuracy=1))+
   theme_modern_rc()+
   theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5),
         legend.position = "bottom", axis.text.y = element_blank(), legend.key.width = unit(2, "cm"))+
   # add those labels
   labs(title = "Cities with >75% of residents within \na 10-minute walk of a park (2020)",
        x = NULL, y = NULL, fill = NULL,
        caption = "data from The Trust for Public Land | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("PublicParkAccess_plot.png", bg = "transparent", width = 8, height = 10, dpi = 400)
