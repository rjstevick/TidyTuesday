# Technology adoption and historical phone data
# TidyTuesday 2020 week 46
# Rebecca Stevick updated 11/10/2020

# Load libraries -----------------
library(tidyverse)
library(ggtext)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-11-10')

# Analysis and plotting ----------
# join mobile data with landline data
tuesdata$mobile %>% full_join(tuesdata$landline) %>%
  # group the subscription numbers into one column
  pivot_longer(c(mobile_subs,landline_subs), names_to="datatype") %>% 
  # drop any missing values and remove the last year since it's incomplete
  drop_na(value) %>% filter(year!=2017) %>% 
  # plot the data
  ggplot(aes(x = datatype, y = value, color = continent, group = code)) +
  # make a panel for each year
  facet_grid(.~year) +
  # add lines for each country
  geom_line(alpha = 0.5, lwd = 1.5, key_glyph = draw_key_rect) +
  # remove extra while space around axes
  scale_y_continuous(expand = c(0,0.2)) + scale_x_discrete(expand = c(0,0)) +
  # change the color scheme
  scale_color_manual(values = nationalparkcolors::park_palette("Arches",5)) +
  # change the overall theme
  theme_minimal() +
  # edit the theme
  theme(text = element_text(family = "Andale Mono"), strip.text = element_text(face = "bold"),
        panel.grid = element_line(color="white"), panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(), legend.position = "bottom", plot.caption = element_text(hjust = 0.5, size = 8, color = "#8c816c"),
        plot.title = element_text(size = 24), plot.subtitle = element_markdown(size=9, family = "Avenir-Black", color = "#8c816c"),
        plot.tag.position = c(0.06, 0.115), plot.tag = element_text(hjust = 1, angle = 50, size = 11, lineheight=1.5)) +
  # add those labels
  labs(title = "Mobile phone plans have replaced landlines over time",
       subtitle = "The transition from landlines to mobile subscriptions started in 1997 in <span style='color:#682C37;'>Asian countries</span>. <span style='color:#9B6981;'>Monaco</span> is the only country with more landlines than mobile plans in 2016.",
       y = "Number of subscriptions per 100 people",
       x = NULL, color = "lines for each country in", tag = "Landline  \nMobile", # use tag as x-axis label
       caption = "data from OurWorldInData.org | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("HistoricalPhones_plot.png", bg = "#e8e6e1", width = 12, height = 7, dpi = 400)
