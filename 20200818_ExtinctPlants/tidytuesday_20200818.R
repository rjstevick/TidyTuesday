# IUCN Extinct Plants threats by continent
# TidyTuesday 2020 week 34
# Rebecca Stevick updated 8/25/2020

# Load libraries
library(tidyverse)
library(ggtext)
library(nationalparkcolors)
library(waffle)
library(hrbrthemes)
library(extrafont)
extrafont::loadfonts()

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-08-18')

tuesdata$threats %>%
  filter(threatened == 1) %>%
  group_by(continent, threat_type) %>% count() %>%
  # divide numbers by 5 for the waffle
  mutate(n5=round(n/5)) %>% ungroup() %>%
  # time to plot!
  ggplot(aes(label = threat_type, colour = threat_type, values = n5))+
  # add pictogram for each threat type
  geom_pictogram(n_rows = 8, size=4, flip = TRUE, family = "FontAwesome5Free-Solid") +
  # separate plots by continent
  facet_wrap(~continent, ncol = 7)+
  # define colors and pictograms
  scale_label_pictogram(name = NULL, values = c("tractor", "tree", "thermometer-three-quarters", "city", 
                                                "lightbulb","cubes", "female", "leaf", 
                                                "house-damage", "smog", "road", "question"))+
  scale_color_manual(name = NULL, values = c(park_palette("Saguaro", n=6), park_palette("SmokyMountains", n=6)))+
  # set themes
  theme_ipsum(grid = "") + theme_enhance_waffle()+
  theme(legend.position = "bottom", strip.text = element_text(face="bold"),
        plot.subtitle = element_markdown(lineheight = 0.5),
        panel.background = element_rect(fill="grey80", color="transparent"),
        panel.spacing.x = unit(0.5, "lines"))+
  # add those labels
  labs(title = "Threatened: Why are plants in danger on each continent?",
       subtitle = "The greatest number of threatened species are in Africa, where the greatest threat is <span style='color:#847CA3;'>Agriculture & Aquaculture</span>. Each symbol represents 5 species.",
       caption = "data from International Union for Conservation of Nature (IUCN) | plot by @rjstevick for #TidyTuesday")


# Saving -----------------------------
ggsave("ExtinctPlants_plot.png", width = 12, height = 6, bg="transparent",dpi = 400)
