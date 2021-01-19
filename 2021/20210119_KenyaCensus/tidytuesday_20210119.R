# Kenya Census
# TidyTuesday 2021 week 4
# Rebecca Stevick updated 1/19/2021

# Load libraries -----------------
library(tidyverse)
library(waffle) # for geom_pictogram
library(hrbrthemes) # for the overall theme
library(extrafont) # for loading the pictogram font

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-01-19')

# Analysis and plotting ----------
tuesdata$crops %>%
  # remove the kenya overall category
  filter(SubCounty!="KENYA") %>% 
  # put all the crops into one column
  pivot_longer(cols=Tea:`Khat (Miraa)`, names_to="crop", values_to="n") %>% 
  # remove any missing data
  drop_na() %>% 
  # divide numbers by 2000 for the waffle so it's not overwhelming. unfortunately, this drops some data
  mutate(nsub=round(n/2000)) %>% 
  # add a column with edited county names
  mutate(countynames= str_to_sentence(SubCounty)) %>% 
  # time to plot!
  ggplot(aes(label = crop, color = crop, values = nsub)) +
  # add pictogram for each crop type. define rows and size of pictogram
  geom_pictogram(n_rows = 10, size = 2, flip = TRUE, family = "FontAwesome5Free-Solid") +
  # separate plots by county put all panels in one row
  facet_wrap(~countynames, ncol = 11) +
  # define pictograms using font awesome icons
  scale_label_pictogram(values = c("dot-circle", "copyright", "lemon", 
                                   "cookie", "coffee", "leaf",
                                   "certificate", "seedling", "bookmark"))+
  # define color palette using PNWcolors
  scale_color_manual(values = c(PNWColors::pnw_palette("Cascades", n = 9)))+
  # set themes from hrbr and waffle
  theme_ipsum(grid = "") + theme_enhance_waffle() +
  # edit the theme
  theme(panel.spacing = unit(0.2, "lines"), strip.text = element_text(face = "bold"),
        legend.position = c(0.86, 0.1), legend.direction = "horizontal",
        legend.text = element_text(size = 11),
        panel.background = element_rect(color = "transparent", fill = "grey90"))+ 
  guides(label = guide_legend(nrow = 3, override.aes = list(size = 4)))+
  # add those labels
  labs(title = "Which Crops Farmed in Kenya?",
       subtitle = "Each icon represents 2000 people growing each crop in 2019",
       color = NULL, label = NULL,
       caption = "data from rKenyaCensus | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("KenyaCensus_plot.png", bg = "transparent", width = 14, height = 8, dpi = 400)
