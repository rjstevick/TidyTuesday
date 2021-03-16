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

logo <- "https://upload.wikimedia.org/wikipedia/en/3/39/Tabletop_Simulator_logo.png"

# Analysis and plotting ----------
games %>% 
   # select only tabletop simulator game data
   filter(gamename == "Tabletop Simulator") %>% 
   # unite month and year columns, then format as a timestamp
   unite(yearmonth, month:year) %>% mutate(date = my(yearmonth)) %>% 
   # start plotting average players over time
   ggplot(aes(x = date)) +
   # add peal players as area background
   geom_area(aes(y = peak), fill = "lightcyan4", color = "lightcyan4") +
   # add average players as light cyan line
   geom_line(aes(y = avg), color = "lightcyan", lwd = 1.2) +
   # add seagreen vertical line at April 2020
   geom_vline(aes(xintercept = as.Date("2020-04-01")), color = "seagreen1") +
   # add logo image at the top right
   geom_image(aes(x = as.Date("2015-12-01"), y = 30000, image = logo), size = 0.35) + scale_size_identity() +
   # Change date labels on y-axis. Put labels at each January
   scale_x_date(date_labels = "%Y", breaks = seq(as.Date("2014-01-01"), as.Date("2022-01-01"), by = "12 months")) +
   # define y-axis breaks and format labels with commas
   scale_y_continuous(breaks = c(0,  10000, 20000,30000, 40000),
                      labels = label_comma(), limits = c(0,NA)) +
   # change global theme
   theme_ft_rc() +
   # edit theme
   theme(plot.title = element_markdown(family = "Futura", lineheight = 1.1, color = "lightcyan"),
         plot.subtitle = element_markdown(color = "grey80"),
         axis.title.y = element_markdown(),
         panel.grid.major = element_line(color = "grey45")) +
   # add those labels
   labs(title = "Online boardgamers quadrupled in <span style='color:seagreen1;'>April 2020</span>", 
        subtitle = "The <span style='color:lightcyan;'>**average number**</span> and <span style='color:lightcyan4;'><b>peak number</b></span> of Tabletop Simulator players were highest during the COVID-19 lockdowns",
        x = NULL, y = "<span style='color:lightcyan;'>Average number of players</span> | <span style='color:lightcyan4;'>Peak players</span>",
        caption = "\ndata from Steam/steamcharts  |  photo from wikipedia  |  plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("VideoGames_plot.png", width = 9.5, height = 6, dpi = 400)
