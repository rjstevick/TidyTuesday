# Australian Animal outcomes
# TidyTuesday 2020 week 30
# Rebecca Stevick updated 7/21/2020

# Load libraries
library(tidyverse)
library(hrbrthemes)
library(waffle) # devtools::install_github("hrbrmstr/waffle")
library(ggtext)

# to get the glyphs to work for the pictogram. help from here https://www.r-craft.org/r-news/quick-hit-waffle-1-0-font-awesome-5-pictograms-and-more/
# install_fa_fonts(); extrafont::font_import(); extrafont::loadfonts(quiet = TRUE)
# extrafont::fonttable() %>% as_tibble() %>% filter(grepl("Awesom", FamilyName)) %>% select(afmfile, FullName, FamilyName, FontName)

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-07-21')
animal_complaints <- tuesdata$animal_complaints

# Let's get piping!
animal_complaints %>%
  # separate date column into month and year
  separate(col="Date Received",sep=" ", into=c("Month","Year")) %>%
  # group and calculate the number of complaints per animal/month/year
  group_by(Year, Month, `Animal Type`) %>% count() %>% 
  # calculate the mean per animal/month
  group_by(Month, `Animal Type`) %>% summarise(meancomplaints=mean(n)) %>%
  # divide mean complaints by 20 for the waffle
  mutate(meancomplaints20=round(meancomplaints/20)) %>% ungroup() %>%
  # add levels to Month so it plots in order
  mutate(Month=factor(Month, levels=c("January","February","March","April","May","June","July",
                                      "August","September","October","November","December"))) %>%
  # time to plot! use example here: https://github.com/hrbrmstr/waffle
  ggplot(aes(label=`Animal Type`, colour = `Animal Type`, values=meancomplaints20))+
  # add pictogram for each animal type
  geom_pictogram(n_rows = 10, size=3, flip = TRUE, family = "FontAwesome5Brands-Regular") +
  # separate plots by month
  facet_wrap(~Month, ncol=4)+
  # define colors and pictograms
  scale_label_pictogram(name=NULL, values = c("cat","dog"), labels = c("Cat","Dog")) +
  scale_color_manual(name = NULL,values = c("#a40000", "#c68958"), labels = c("Cat","Dog")) +
  # set theme
  theme_ipsum_rc(grid="") + theme_enhance_waffle()+theme(legend.position = "none", plot.subtitle = element_markdown(lineheight = 0.5), strip.text = element_text(face="bold"))+
  # add those labels
  labs(title= "Average animal complaints per month in Australia (1999-2017)",
       subtitle="Each <span style='color:#c68958;'>**dog**</span> or <span style='color:#a40000;'>**cat**</span> represents 20 complaints",
       caption = "Source: Royal Society for the Prevention of Cruelty to Animals | Plot by @rjstevick for #TidyTuesday")

# Saving -----------------------------
ggsave("AustralianAnimals_plot.png", width = 5, height = 2, dpi=400)
