# International Independence Days
# TidyTuesday 2021 week 28
# Rebecca Stevick updated 8/5/2021

# Load libraries -----------------
library(tidyverse)
library(ggstream)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-07-06')
holidays <- tuesdata$holidays

# Analysis and plotting ----------
holidays %>%
   filter(year > 1750) %>% drop_na(year) %>% 
   # select top 8 countries and group all others into an "Others" category
   mutate(independenceother = fct_lump_n(independence_from, 8)) %>%
   # count number of countries per year and independence from
   group_by(year, independenceother) %>% count() %>% 
   # start plotting
   ggplot(aes(x=year, y=n, fill=independenceother)) +
   geom_stream(color = "grey20", lwd = 0.1, alpha = 0.7, bw = 0.2, type = "ridge") +
   scale_fill_manual(values = c(PNWColors::pnw_palette("Shuksan", 4), PNWColors::pnw_palette("Lake", 4), "grey")) +
   theme_ipsum() +
   theme(legend.position = c(0.35,0.8), plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5),
         panel.grid.minor = element_blank(), panel.grid.major.y = element_blank()) +
   guides(fill = guide_legend(ncol = 3)) +
   # add those labels
   labs(title = "World Independence Days since 1750",
        subtitle = "an overview of when countries celebrate their independence days, and from whom they obtained independence",
        fill = "Independence from...", x = NULL, y = "Number of countries per year", 
        caption = "data from Wikipedia  |  plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("IndependenceDays_plot.png", bg = "transparent", width = 9, height = 5, dpi = 400)
