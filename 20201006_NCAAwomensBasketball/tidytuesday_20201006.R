# NCAA Women's Basketball - UMD!
# TidyTuesday 2020 week 41
# Rebecca Stevick updated 10/7/2020

# Load libraries ---------------
library(tidyverse)
library(hrbrthemes)
library(emojifont)
extrafont::loadfonts()

# Load data --------------------
tuesdata <- tidytuesdayR::tt_load('2020-10-06')

# Analysis and plotting --------
tuesdata$tournament %>%
  # select only UMD
  filter(school=="Maryland") %>%
  # group the wins and losses into one column
  pivot_longer(full_w:full_l) %>%
  # add text for the years with tournament finishes
  mutate(win=case_when(tourney_finish == "Champ" ~ "Champions!",
                       tourney_finish == "NSF" ~ "Final Four",
                       tourney_finish == "RF" ~ "Elite Eight",
                       tourney_finish == "RSF" ~ "Sweet Sixteen")) %>%
  # add glyphs for the years with tournament finishes
  mutate(winpict=fontawesome(case_when(tourney_finish == "Champ" ~ "fa-trophy",
                       tourney_finish == "NSF" ~ "fa-asterisk",
                       tourney_finish == "RF" ~ "fa-bandcamp",
                       tourney_finish == "RSF" ~ "fa-birthday-cake"))) %>%
  # start plotting
  ggplot(aes(x=year, y=value, fill=name))+
  # add barplot
  geom_col(position="fill", color="white")+
  # add glyph icons to the top
  geom_text(aes(color=win, label=winpict, y=1.05), family = 'fontawesome-webfont', size=4, key_glyph=draw_key_point)+
  # change bar and glyph colors
  scale_fill_manual(values=c("red3", "gold2"), labels=c("Loss", "Win"))+
  scale_color_manual(values=c("black", "red3", "gold2", "grey"), limits=c("Champions!","Final Four","Elite Eight","Sweet Sixteen"))+
  # add percent labels and change theme
  scale_y_continuous(labels=scales::percent_format()) + theme_ipsum()+
  # add those labels
  labs(x=NULL, y=NULL, fill="Overall Season \nWin or Loss", color="Tournament Results",
       title="University of Maryland NCAA Women's Basketball record",
       caption="data from FiveThirtyEight | plot by @rjstevick for #TidyTuesday")

# Saving -----------------------
ggsave("NCAAwomensBasketball_plot.png", bg="transparent", width = 10, height = 5, dpi = 400)
