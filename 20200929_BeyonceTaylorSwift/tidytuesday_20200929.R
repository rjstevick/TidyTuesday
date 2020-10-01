# Beyonce & Taylor Swift Lyrics
# TidyTuesday 2020 week 40
# Rebecca Stevick updated 9/29/2020

# Load libraries ---------------
library(tidyverse)
library(hrbrthemes)
library(tidytext)

# Load data --------------------

beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')
sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')
charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')

# Analysis and plotting ------

beyonce_lyrics %>% 
  unnest_tokens(word, line) %>% 
  group_by(word) %>% 
  anti_join(stop_words) %>% 
  count(sort=TRUE) %>% ungroup() %>% 
  slice_head(n=50) %>% 
  ggplot(aes(x=reorder(word,n), y=n)) +
  geom_col() + coord_flip() + 
  theme_minimal()
  
sales %>% 
  filter(artist=="Beyoncé" & country=="World") 

  # add those labels
  labs(x=NULL, y=NULL,
       title="Nepalese climbers have participated in the most first ascents of the Himalayas",
       subtitle="Timeline of first ascents of Himalayan Peaks by country",
       caption="data from       | plot by @rjstevick for #TidyTuesday")

# Saving -----------------------
ggsave("BeyonceTaylorSwift_plot.png", bg="transparent", width = 12, height = 6.5, dpi = 400)
