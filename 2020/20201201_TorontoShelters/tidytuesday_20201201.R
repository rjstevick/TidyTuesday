# Toronto homeless shelters
# TidyTuesday 2020 week 49
# Rebecca Stevick updated 12/01/2020

# Load libraries -----------------
library(tidyverse)
library(ggalt)
library(lubridate)
library(ggtext)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-12-01')

# Analysis and plotting ----------
tuesdata$shelters %>%
  # add a month-year variable from the occupancy_date
  mutate(monthyear = as.yearmon(format(occupancy_date, "%Y-%m"))) %>%
  # calculate sum of the occupancy and capacity per month
  group_by(monthyear) %>%
  summarise(sumoccupancy=sum(occupancy), sumcapacity=sum(capacity, na.rm=TRUE)) %>%
  # filter months where the capacity was 0
  filter(sumcapacity != 0) %>%
  # calculate the difference between the capacity and occupancy
  mutate(diff=sumcapacity-sumoccupancy) %>%
  # start plotting
  ggplot(aes(y = monthyear, x = sumoccupancy, xend = sumcapacity))+
  # add dumbbells based on occupancy and capacity
  geom_dumbbell(aes(color=diff), size = 4, shape=16,
                colour_x = "#81a9ad", colour_xend = "#2d2926")+
  # change color scheme
  scale_color_gradient(low="#f5f5f5", high="#537380")+
  # change the overall theme and flip axis
  theme_ipsum() + coord_flip()+
  # edit the theme
  theme(legend.position = "none",
        plot.title = element_markdown(lineheight = 1.1, color="grey50"),
        plot.subtitle = element_markdown(lineheight = 0.5))+
  # add those labels
  labs(x=NULL, y=NULL, title="Toronto homeless shelter <span style='color:#2d2926;'>capacity</span> and <span style='color:#81a9ad;'>occupancy</span> have increased over time",
       caption = "data from opendatatoronto | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("TorontoShelters_plot.png", bg = "transparent", width = 10, height = 5, dpi = 400)
