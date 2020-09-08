# Global crop yields
# TidyTuesday 2020 week 37
# Rebecca Stevick updated 9/8/2020

# Load libraries ---------------
library(tidyverse)

# Load data --------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-08')
friends <- tuesdata$friends
friends_emotions <- tuesdata$friends_emotions %>%
  left_join(friends)

# Analysis and formatting ------

friends_emotions %>%
  filter(speaker %in% c("Ross Geller", "Monica Geller", "Chandler Bing", "Joey Tribbiani", "Rachel Green")) %>% 
  group_by(speaker, season, episode, emotion) %>% 
  count() %>% 
  ggplot(aes(x=episode, y=speaker, color=emotion, size=n))+
  facet_grid(~season)+
  geom_jitter()

friends_emotions %>%
  filter(speaker %in% c("Ross Geller", "Monica Geller", "Chandler Bing", "Joey Tribbiani", "Rachel Green")) %>% 
  filter(emotion != "Neutral") %>% 
  group_by(speaker, emotion) %>% 
  count() %>% 
  ggplot(aes(x=emotion, y=n, fill=speaker))+
  geom_col(position="dodge")+facet_grid(~emotion, scales = "free")





  # add those labels
  labs(title="",
       subtitle="",
       caption="data from friends R package (Emil Hvitfeldt) | plot by @rjstevick for #TidyTuesday")


# Saving -----------------------
ggsave("Friends_plot.png", bg = "transparent", width = 12, height = 6.5, dpi = 400)
