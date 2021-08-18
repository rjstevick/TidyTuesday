# Paralympic Medals
# TidyTuesday 2021 week 32
# Rebecca Stevick updated 8/3/2021

# Load libraries -----------------
library(tidyverse)
library(ggtext)
library(hrbrthemes)
library(waffle)
library(extrafont)
loadfonts()

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-08-03')
athletes <- tuesdata$athletes

# Analysis and plotting ----------
athletes %>%
   # filter only USA data since 1999
   filter(abb=="USA") %>% 
   # count each medal type per year
   group_by(medal, year) %>% drop_na(medal) %>%  count() %>% 
   # reorder the medal as a factor
   mutate(medal=factor(medal, levels=c("Gold", "Silver", "Bronze"))) %>% 
   # divide counts by 5 for the waffle
   mutate(n5=ceiling(n/5))  %>% ungroup() %>% 
   # start plotting
   ggplot(aes(values=n5, color=medal, label=medal))+
   # add pictograms
   geom_pictogram(n_rows = 6, size = 3.5, flip = TRUE, family = "FontAwesome5Free-Solid") +
   # make a panel per year
   facet_grid(~year, switch="x")+
   # define color and pictogram icon
   scale_color_manual(values = c("#c9b037", "#b4b4b4", "#ad8a56"))+
   scale_label_pictogram(values = c("medal", "medal", "medal")) +
   # change global theme
   theme_ipsum(grid="") + theme_enhance_waffle()+
   # edit theme
   theme(legend.position="none", panel.grid.major.x = element_blank(),
         plot.subtitle = element_markdown(lineheight=1.1), plot.title = element_text(size=22),
         panel.spacing.x = unit(-0.01, "lines"), strip.text=element_text(face="bold", size=18))+
   # add those labels
   labs(title = "USA Paralympic Medals",
        subtitle = "Number of <span style='color:#c9b037;'>**GOLD**</span>, <span style='color:#b4b4b4;'>**SILVER**</span>, and 
        <span style='color:#ad8a56;'>**BRONZE**</span> medals earned at each paralympic games since 1980. Each medal icon represents 5 medals.",
        x = NULL, y = "Number of medals",
        caption = "data from International Paralympic Committee | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("ParalympicMedals_plot.png", bg = "transparent", width = 10, height = 5, dpi = 400)

