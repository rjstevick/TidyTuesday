# WEB Du Bois challenge tweet locations
# TidyTuesday 2021 week 25
# Rebecca Stevick updated 8/3/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)
library(emojifont)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-06-15')
tweets <- tuesdata$tweets

# Analysis and plotting ----------
ggplot() +
   # creat world map
   geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group),
                fill = "grey80", color = "gray90", lwd = 0.3) +
   # add a blue twitter bird per tweet, with white outline
   geom_text(data=tweets, aes(x = long, y = lat), color = "white",
             family = 'fontawesome-webfont', label = fontawesome("fa-twitter"), size = 2.5) +
   geom_text(data=tweets, aes(x = long, y = lat), color = "dodgerblue",
             family = 'fontawesome-webfont', label = fontawesome("fa-twitter"), size = 2) +
   # set global theme and fix coordinates
   theme_ft_rc() + coord_fixed(1.3) +
   # edit theme
   theme(plot.title = element_text(size=12),
         axis.text.x = element_blank(), axis.text.y = element_blank()) +
   # add those labels
   labs(x = NULL, y = NULL,
        title = "Locations of tweets for the #DuBoisChallenge in 2021",
        caption = "data from #DuBoisChallenge tweets | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("WEBduBois_plot.png", bg = "transparent", width = 8, height = 5, dpi = 400)
