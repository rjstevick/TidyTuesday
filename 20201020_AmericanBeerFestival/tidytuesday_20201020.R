# Great American Beer Festival map/pictogram
# TidyTuesday 2020 week 43
# Rebecca Stevick updated 10/20/2020

# Load libraries -----------------
library(tidyverse)
library(geofacet)
library(emojifont)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-10-20')

# Analysis and plotting ----------
# help from here https://stackoverflow.com/questions/59593058/how-to-make-a-faceted-waffle-chart-filling-each-bin-from-top-left

# let's get piping!
tuesdata$beer_awards %>% 
  # fix some state names
  mutate(state=recode(state, "Ak"="AK", "wa"="WA")) %>% 
  # count number of awards earned per state
  group_by(state, medal) %>% count() %>% 
  # order the medal as a factor
  mutate(medal=factor(medal, levels=c("Gold","Silver","Bronze"))) %>% 
  # divide counts by 10 and then make a row for each state/medal combo based on this number
  mutate(n10=ceiling(n/10)) %>% uncount(n10) %>% 
  # order by state and then group
  arrange(medal) %>% group_by(state) %>% 
  # make a waffle guide for the points, based on 8 rows/columns
  mutate(num = row_number(), x_pos = (num - 1) %/% 8, y_pos = 8 - (num - 1) %% 8 - 1) %>%
  # time to plot
  ggplot(aes(x=x_pos, y=y_pos, colour=medal)) + 
  # add the icons as points, select fa-beer as the icon
  geom_text(family = 'fontawesome-webfont', label=fontawesome("fa-beer"), size=3) +
  # make a panel for each state in its geographical location (relatively)
  facet_geo(~state) + 
  # change icon colors
  scale_color_manual(name=NULL,values=c("gold2","#C0C0C0","#CD7F32")) +
  # change global theme
  theme_void() +
  # edit theme
  theme(text=element_text(size=18), plot.title=element_text(face="bold", size=30), 
        strip.text = element_text(face="bold"),
        panel.background=element_rect(fill="cornsilk", color="transparent"),
        legend.position=c(0.92,0.2), legend.text=element_text(size=18)) + 
  # make the legend steins bigger
  guides(colour = guide_legend(override.aes = list(size=10))) +
  # add those labels
  labs(x=NULL, y=NULL, title="Which states produce the best beer?", 
       subtitle="Great American Beer Festival awards earned per state since 1987. Each stein represents up to 10 awards.",
       caption="data from Great American Beer Festival | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("AmericanBeerFestival_plot.png", bg="transparent", width = 14, height = 8, dpi = 400)
