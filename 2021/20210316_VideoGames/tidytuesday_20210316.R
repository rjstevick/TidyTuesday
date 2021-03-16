# Video Games + Sliced - tabletop simulator popularity over time
# TidyTuesday 2021 week 12
# Rebecca Stevick updated 3/16/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes) # for the global theme
library(ggtext) # to color the title text
library(ggimage) # to add the logo
library(lubridate) # formatting datestamps
library(scales) # format plot axes

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-03-16')
games <- tuesdata$games

#logo <- "http://berserk-games.com/new/wp-content/uploads/2015/02/TTS-Horizontal-Logo.png"
logo <- "https://upload.wikimedia.org/wikipedia/en/3/39/Tabletop_Simulator_logo.png"

# Analysis and plotting ----------
games %>% 
   # select only tabletop simulator game data
   filter(gamename == "Tabletop Simulator") %>% 
   # unite month and year columns, then format as a timestamp
   unite(yearmonth, month:year) %>% mutate(date = my(yearmonth)) %>% 
   # start plotting average players over time
   ggplot(aes(x = date, y = avg)) +
   # add light cyan line
   geom_line(color = "lightcyan", lwd = 1.2) +
   # add seagreen vertical line at April 2020
   geom_vline(aes(xintercept = as.Date("2020-04-01")), color = "seagreen1") +
   # add logo image at the top right
   geom_image(aes(x = as.Date("2016-03-01"), y = 11000, image = logo), size = 0.4) + scale_size_identity() +
   # Change date labels on y-axis. Put labels at each January
   scale_x_date(date_labels = "%Y", breaks = seq(as.Date("2014-01-01"), as.Date("2022-01-01"), by = "12 months")) +
   # define y-axis breaks and format labels with commas
   scale_y_continuous(breaks = c(0, 2500, 5000, 7500, 10000, 12500), labels = label_comma(), limits = c(0,NA)) +
   # change global theme
   theme_ft_rc() +
   # edit theme
   theme(plot.title = element_markdown(family = "Futura", lineheight = 1.1, color = "lightcyan"),
         panel.grid.major = element_line(color = "grey45")) +
   # add those labels
   labs(title = "Online boardgamers quadrupled in <span style='color:seagreen1;'>April 2020</span>", 
        subtitle = "Tabletop Simulator exploded in popularity during the COVID-19 lockdowns",
        x = NULL, y = "Average number of players",
        caption = "data from Steam/steamcharts  |  photo from wikipedia  |  plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("VideoGames_plot.png", width = 10, height = 6, dpi = 400)
