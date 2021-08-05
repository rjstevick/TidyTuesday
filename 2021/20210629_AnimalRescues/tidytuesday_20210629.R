# London Animal Rescues
# TidyTuesday 2021 week 27
# Rebecca Stevick updated 8/4/2021

# Load libraries ---------------
library(tidyverse) # for general data manipulation
library(ggtext) # to add colored text to the plot
library(nationalparkcolors) # for discrete color schemes
library(waffle) # for geom_pictogram
library(hrbrthemes) # for the overall theme
library(extrafont) # for loading the pictogram font
loadfonts() # load fonts into the R session (works on Mac, Windows is harder and needs extra steps)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-06-29')
animal_rescues <- tuesdata$animal_rescues

# Analysis and plotting ----------
animal_rescues %>%
   # clean up animal category
   mutate(animal_group_parent = case_when(animal_group_parent == "cat" ~ "Cat",
                                          grepl("Unknown", animal_group_parent) ~ "Unknown",
                                          TRUE ~ animal_group_parent)) %>%
   # count number of animals per year
   group_by(cal_year, animal_group_parent) %>% count() %>%
   # filter only animals with more than 10 rescued total
   filter(sum(n)>10) %>%
   # divide counts by 10 for the waffle
   mutate(n10=ceiling(n/10))  %>% ungroup() %>%
   ggplot(aes(label=animal_group_parent, values=n10, color=animal_group_parent))+
   geom_pictogram(n_rows = 8, size = 3.5, flip = TRUE, family = "FontAwesome5Free-Solid")+
   # separate plots by month
   facet_wrap(.~cal_year, ncol=5)+
   # add pictogram for each animal type
   scale_label_pictogram(name = NULL, values = c("crow", "cat", "leaf", "dog",
                                                 "certificate", "horse", "tree", "question")) +
   # define color palette using nationalparkcolors
   scale_color_manual(name = NULL, values = c(rev(park_palette("ChannelIslands", n=6)), "brown", "darkgrey"))+
   # set themes
   theme_ipsum(grid="") + theme_enhance_waffle()+
   theme(legend.position = c(0.8,0.15), legend.text = element_text(size = 15, margin = margin(t = 5, b = 15, r = 10)),
         panel.spacing = unit(0.2, "lines"),  plot.margin = margin(10, 10, 10, 10),
         strip.text = element_text(face="bold", size=18), plot.title = element_text(size = 24))+
   # put legend items in 2 columns
   guides(label = guide_legend(ncol = 3, override.aes = list(size=8))) +
   # add those labels
   labs(title = "Most Common Animal Rescues in London, UK",
        subtitle = "Each animal represents up to 10 rescues. Cats are the most commonly rescued animal every year",
        caption = "data from The London Fire Brigade via london.gov | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("AnimalRescues_plot.png", bg = "antiquewhite", width = 11, height = 7, dpi = 400)
