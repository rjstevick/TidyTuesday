# Olympic Medals
# TidyTuesday 2021 week 31
# Rebecca Stevick updated 7/27/2021

# Load libraries -----------------
library(tidyverse)
library(ggtext)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-07-27')
olympics <- tuesdata$olympics

# Analysis and plotting ----------
olympics %>% 
   # filter only USA data since 1999
   filter(noc=="USA" & year > 1999) %>% 
   # count number of each medal type per season/year
   group_by(year, season, medal) %>% drop_na(medal) %>% count() %>% 
   # reorder the medal category
   mutate(medal=factor(medal, levels=c("Gold", "Silver", "Bronze"))) %>% 
   # start plotting, make a panel per season/year
   ggplot() + facet_grid(season~year)+
   # add barplot
   geom_col(aes(y=n, x=1, fill=medal), position="fill", color="white")+
   # add year to the center of the barplot
   geom_text(aes(x=-3, y=0, label=year, color=season), size=10)+
   # turn into donut plot
   coord_polar(theta="y", clip="off") + xlim(c(-3, 2))+
   # define colors for rings and years
   scale_fill_manual(values = c("#c9b037", "#b4b4b4", "#ad8a56"))+
   scale_color_manual(values=c("coral2", "navy"))+
   # add global theme
   theme_void()+
   # edit theme
   theme(text = element_text(family = "Avenir"), strip.text = element_blank(), 
         # change panel spacing so the rings overlap
         panel.spacing = unit(-8, "lines"),
         legend.position = "none", plot.title = element_text(hjust=0.5, size=28), 
         plot.subtitle = element_markdown(lineheight=1.4, hjust=0.5), 
         plot.caption = element_text(hjust=0.5))+
   # add those labels
   labs(title = "USA Olympic Medals",
        subtitle = "Each ring shows the percent of each medal type earned per <span style='color:coral2;'>**SUMMER**</span> or <span style='color:navy;'>**WINTER**</span> olympic games since 2000.<br>
        Most medals earned at the summer games are <span style='color:#c9b037;'>**GOLD**</span>, and <span style='color:#b4b4b4;'>**SILVER**</span> is the most commonly earned medal at the winter games.",
        caption = "data from Kaggle | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("OlympicMedals_plot.png", bg = "transparent", width = 10, height = 6, dpi = 400)
