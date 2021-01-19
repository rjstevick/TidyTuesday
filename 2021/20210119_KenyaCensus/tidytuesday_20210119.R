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
  filter(SubCounty!="KENYA") %>% 
  pivot_longer(cols=Tea:`Khat (Miraa)`, names_to="crop", values_to="n") %>% 
  drop_na() %>% 
  # divide numbers by 5 for the waffle so it's not overwhelming. unfortunately, this drops species <2... 
  mutate(n5=round(n/2000)) %>% 
  mutate(countynames= str_to_sentence(SubCounty)) %>% 
  # time to plot!
  ggplot(aes(label = crop, color = crop, values = n5)) +
  # add pictogram for each threat type. define rows and size of pictogram
  geom_pictogram(n_rows = 10, size = 4, flip = TRUE, family = "FontAwesome5Free-Solid") +
  # separate plots by continent. put all panels in one row
  facet_wrap(~countynames, ncol = 11) +
  # define pictograms using font awesome icons
  scale_label_pictogram(values = c("tractor", "tree", "thermometer-three-quarters", "city",
                                                "lightbulb", "cubes", "female", "leaf",
                                                "house-damage", "smog", "road", "question"))+
  # define color palette using nationalparkcolors
  scale_color_manual(values = c(PNWColors::pnw_palette("Cascades", n=9)))+
  # set themes from hrbr and waffle
  theme_ipsum(grid = "") + theme_enhance_waffle() +
  theme(panel.spacing = unit(0, "lines"), strip.text = element_text(face="bold"),
        legend.position = c(0.86, 0.1), legend.direction = "horizontal")+ 
  guides(label = guide_legend(nrow = 3))+
  # add those labels
  labs(title = "Where are Crops Farmed in Kenya?",
       subtitle = "Each icon represents 2000 people growing each crop in 2019",
       color = NULL, label = NULL,
       caption = "data from rKenyaCensus | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("KenyaCensus_plot.png", bg = "transparent", width = 9, height = 5, dpi = 400)
