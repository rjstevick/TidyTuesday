# Beyonce & Taylor Swift Lyrics - focus on Beyonce
# TidyTuesday 2020 week 40
# Rebecca Stevick updated 10/6/2020

# Load libraries ---------------
library(tidyverse)
library(tidytext)
library(ggimage)
library(beyonce) # devtools::install_github("dill/beyonce")
extrafont::loadfonts()

# Load data --------------------

beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
musicnote<-"https://www.transparentpng.com/thumb/musical-notes/musical-notes-png-0.png"

# Analysis and plotting ------

beyonce_lyrics %>%
  # separate each song line by word
  unnest_tokens(word, line) %>%
  # group by word and remove stop words
  group_by(word) %>% anti_join(stop_words) %>%
  # count number of times each word occurs, then select just the top 30
  count(sort=TRUE) %>% ungroup() %>% slice_head(n=30) %>%
  # start plotting
  ggplot(aes(x=reorder(word,n), y=n, fill=n)) +
  # add bars and text
  geom_col() + geom_text(aes(label=word, y=n/2), color="white", family="Andale Mono") +
  # change color scheme
  scale_fill_gradientn(colours=beyonce_palette(27, type = "continuous")) +
  # switch axes and add theme
  coord_flip() + theme_minimal() +
  # edit the theme
  theme(axis.text.y = element_blank(), axis.title.x=element_text(hjust=1), legend.position="none",
        text = element_text(family="Andale Mono"))+
  # add the music note image
  geom_image(aes(x=11, y=1000, image=musicnote), size=0.6)+scale_size_identity()+
  # add those labels
  labs(y="Number of times word was sung", x=NULL,
       title="Most common lyrics in Beyoncé songs",
       caption="data from Dr. Sara Stoudt | plot by @rjstevick for #TidyTuesday")

# Saving -----------------------
ggsave("BeyonceTaylorSwift_plot.png", bg="transparent", width = 7, height = 5, dpi = 400)
