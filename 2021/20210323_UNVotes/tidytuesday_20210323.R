# UN Votes
# TidyTuesday 2021 week 13
# Rebecca Stevick updated 3/23/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-03-23')
unvotes <- tuesdata$unvotes
issues <- tuesdata$issues
roll_calls <- tuesdata$roll_calls

# Analysis and plotting ----------
issues %>% 
  # join all data together
  left_join(unvotes) %>% left_join(roll_calls) %>% 
  # select countries
  filter(country %in% c("United States","Russia","France","China","Germany","Australia")) %>% 
  # wrap/edit label text
  mutate(country = str_wrap(country, 10), issue = str_wrap(issue, 15),
         vote = str_to_sentence(vote)) %>% 
  # start plotting
  ggplot(aes(x = issue, y = country, color = vote)) +
  # panel for country vs issue
  facet_grid(country~issue, scales = "free", space = "free") +
  # add points
  geom_jitter(alpha = 0.6, size = 3) +
  # define colors
  scale_color_manual(values = c("grey40","salmon1","darkturquoise")) +
  # change global theme
  theme_ipsum() +
  # edit theme
  theme(legend.position = "top", legend.text = element_text(size = 18),
        plot.title = element_text(size = 30, hjust = 0.5), plot.caption = element_text(size = 12),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_text(hjust = 0.5, face="bold", size = 15),
        strip.text.x = element_text(hjust = 0.5, face="bold", size = 22),
        strip.text.y = element_blank(), panel.spacing = unit(0, "lines")) +
  # add those labels
  labs(x = NULL, y = NULL, color = NULL,
       title = "UN Votes of select countries by category",
       caption = "data from Harvard Dataverse  |  plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("UNVotes_plot.png", bg = "transparent", width = 14, height = 8, dpi = 400)
