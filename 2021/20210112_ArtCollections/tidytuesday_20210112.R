# Art Collections
# TidyTuesday 2021 week 3
# Rebecca Stevick updated 1/15/2021

# Load libraries -----------------
library(tidyverse)
library(ggstream)
library(hrbrthemes)
library(nationalparkcolors)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-01-12')

# Analysis and plotting ----------
tuesdata$artwork %>%
  # filter out oldest data and drop any missing data
  filter(year >= 1740) %>%  drop_na(medium) %>%
  # select top 15 mediums and group all others into an "Others" category
  mutate(mediumother = fct_lump_n(medium, 15)) %>%
  # count number of artworks per medium per year
  group_by(year, mediumother) %>% count() %>%
  # start plotting. order medium by the total number
  ggplot(aes(x = year, y = n, fill = reorder(mediumother,n))) +
  # add geom_stream of the mediums
  geom_stream(bw = 0.4, color = "white", alpha = 0.7) +
  # define the color scheme
  scale_fill_manual(values = c(park_palette("Redwoods"), park_palette("GeneralGrant"), park_palette("CraterLake")))+
  # set the overall theme
  theme_ft_rc() +
  # edit theme elements
  theme(legend.position = c(0.72,0.88), legend.direction = "horizontal",
        legend.text = element_text(size=8), plot.title = element_text(size=32),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+
  # add those labels
  labs(title = "The Art of Embracing Change",
       subtitle = "Evolution of art styles in the Tate Collection since the 1700s",
       fill = NULL, x = NULL, y = "Total number of works of art",
       caption = "data from Tate Art Museum | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("ArtCollections_plot.png", width = 13, height = 7.5, dpi = 400)
