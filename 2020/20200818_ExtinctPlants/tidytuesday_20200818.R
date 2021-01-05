# IUCN Extinct Plants threats by continent
# TidyTuesday 2020 week 34
# Rebecca Stevick updated 8/27/2020

# Load libraries ---------------
library(tidyverse) # for general data manipulation
library(ggtext) # to add colored text to the plot
library(nationalparkcolors) # for discrete color schemes
library(waffle) # for geom_pictogram
library(hrbrthemes) # for the overall theme
library(extrafont) # for loading the pictogram font
loadfonts() # load fonts into the R session (works on Mac, Windows is harder and needs extra steps)

# Load data --------------------
tuesdata <- tidytuesdayR::tt_load('2020-08-18')

# Analysis and plotting --------
tuesdata$threats %>%
  # select only rows with data
  filter(threatened == 1) %>%
  # sum up number of threats per type and continent
  group_by(continent, threat_type) %>% count() %>%
  # divide numbers by 5 for the waffle so it's not overwhelming. unfortunately, this drops species <2... 
  mutate(n5=round(n/5)) %>% ungroup() %>%
  # time to plot!
  ggplot(aes(label = threat_type, colour = threat_type, values = n5))+
  # add pictogram for each threat type. define rows and size of pictogram
  geom_pictogram(n_rows = 8, size = 4, flip = TRUE, family = "FontAwesome5Free-Solid")+
  # separate plots by continent. put all panels in one row
  facet_wrap(~continent, ncol = 7)+
  # define pictograms using font awesome icons
  scale_label_pictogram(name = NULL, values = c("tractor", "tree", "thermometer-three-quarters", "city",
                                                "lightbulb", "cubes", "female", "leaf",
                                                "house-damage", "smog", "road", "question"))+
  # define color palette using nationalparkcolors
  scale_color_manual(name = NULL, values = c(park_palette("Saguaro", n=6), park_palette("SmokyMountains", n=6)))+
  # set themes from hrbr and waffle
  theme_ipsum(grid = "") + theme_enhance_waffle()+
  # edit themes
  theme(legend.position = "bottom", # put legend at bottom of plot
        strip.text = element_text(face="bold"), # make the continent names bold
        plot.subtitle = element_markdown(lineheight = 0.5), # subtitle as markdown so I can add color
        panel.background = element_rect(fill="grey80", color="transparent"), # add grey background with no border to panels
        panel.spacing.x = unit(0.5, "lines"))+ # decrease space between panels
  # add those labels
  labs(title = "Threatened: Why are plants in danger on each continent?",
       subtitle = "The greatest number of threatened species are in Africa, where the greatest threat is 
       <span style='color:#847CA3;'>**Agriculture & Aquaculture**</span>. Each symbol represents 5 species.",
       caption = "data from International Union for Conservation of Nature (IUCN) | plot by @rjstevick for #TidyTuesday")

# Saving ------------------------
ggsave("ExtinctPlants_plot.png", width = 11.5, height = 6, bg="transparent",dpi = 400)
