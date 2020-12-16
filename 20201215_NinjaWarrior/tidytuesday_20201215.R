# Ninja Warrior
# TidyTuesday 2020 week 51
# Rebecca Stevick updated 12/15/2020

# Load libraries -----------------
library(tidyverse)
library(ggrepel)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-12-15')

# Analysis and plotting ----------
tuesdata$ninja_warrior %>%
  # count number of times each obstacle was in each position
  group_by(obstacle_name, obstacle_order) %>% count(sort=TRUE) %>%
  # select the top-occuring obstacle for each position in the course
  group_by(obstacle_order) %>% top_n(n = 1, wt = n) %>% 
  ggplot(aes(x=obstacle_order, y=n)) +
  geom_step(color="#2980B9", size=3) + geom_point(color="#2980B9", size=5) +
  geom_label_repel(aes(label=obstacle_name), fill="#E74C3C", family="Cinzel Black", segment.colour = NA) +
  scale_y_continuous(limits=c(0,NA)) +
  theme_minimal() +
  theme(text=element_text(family="Titillium Web", size=16),
        plot.title=element_text(family="Cinzel Black", hjust=0.5, size=28, color="#1A5276"),
        plot.subtitle=element_text(family="Cinzel", hjust=0.5, color="#2980B9"))+
  # add those labels
  labs(x=NULL, y="Frequency of obstacle",
       title = "The most common Ninja Warrior course", 
       subtitle="Averaged across 10 seasons of courses",
       caption = "data from the Data.World | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("NinjaWarrior_plot.png", bg = "transparent", width = 11, height = 5, dpi = 400)
