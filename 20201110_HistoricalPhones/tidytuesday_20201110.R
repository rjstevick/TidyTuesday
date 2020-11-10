# Technology adoption and historical phone data
# TidyTuesday 2020 week 46
# Rebecca Stevick updated 11/10/2020

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-11-10')

# Analysis and plotting ----------
# join mobile data with landline data
tuesdata$mobile %>% full_join(tuesdata$landline) %>% 
  # group the subscription numbers into one column. drop any missing values
  pivot_longer(c(mobile_subs,landline_subs), names_to="datatype") %>% drop_na(value) %>%
  # plot the data
  ggplot(aes(x = datatype, y = value, color = continent, group = code)) +
  # make a panel for each year
  facet_grid(.~year) +
  # add lines for each country
  geom_line(alpha = 0.5, lwd = 1.5, key_glyph = draw_key_rect) +
  # remove extra while space around axes
  scale_y_continuous(expand = c(0,0)) + scale_x_discrete(expand = c(0,0)) + 
  # change the color scheme
  scale_color_manual(values = nationalparkcolors::park_palette("Arches",5)) +
  # change the overal theme
  theme_minimal() +
  # edit the theme
  theme(text = element_text(family = "Andale Mono"), strip.text = element_text(face = "bold"), panel.grid = element_line(color="white"),
        axis.text.x = element_blank(), legend.position = "bottom", plot.caption = element_text(hjust = 0.5, size = 8, color = "#8c816c"),
        plot.title = element_text(size = 24), plot.subtitle = element_text(size=9, family = "Avenir-Black", color = "#8c816c"),
        plot.tag.position = c(0.055, 0.11), plot.tag = element_text(hjust = 1, angle = 60, size = 11)) +
  # add those labels
  labs(title = "Mobile phone plans have replaced landlines over time",
       subtitle = "The transition from landlines to mobile subscriptions started in 1999 in Asia. Monaco is the only country with more landlines than mobile plans in 2017.",
       y = "Number of subscriptions per 100 people",
       x = NULL, color = NULL, tag = "Landline  \n\nMobile",
       caption = "data from OurWorldInData.org | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("HistoricalPhones_plot.png", bg = "#e8e6e1", width = 12, height = 7, dpi = 400)


