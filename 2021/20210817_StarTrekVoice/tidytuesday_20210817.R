# Star Trek Voice Commands
# TidyTuesday 2021 week 34
# Rebecca Stevick updated 8/17/2021

# Load libraries -----------------
library(tidyverse)
library(tidytext)
library(ggtext)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-08-17')
computer <- tuesdata$computer

# Analysis and plotting ----------
computer %>%
   # separate each interaction by word
   unnest_tokens(word, interaction) %>%
   # group by word and remove stop words
   group_by(word, char_type) %>% anti_join(stop_words) %>%
   # count number of times each word occurs, then select just the top 10
   count(sort=TRUE) %>% group_by(char_type) %>% slice_max(n=10, order_by=n) %>% 
   # start plotting
   ggplot() +
   # make a panel per computer or person
   facet_grid(.~char_type)+
   # add bars and text
   geom_col(aes(x=reorder(word,n), y=n, fill=char_type)) + 
   geom_text(aes(x=reorder(word,n), label=word, y=n, color=char_type), 
              family="Andale Mono", size=6, hjust=0) +
   # add line at 0
   geom_hline(yintercept=0, color="white", lwd=2)+
   # change color scheme
   scale_fill_manual(values=c("cyan", "green1"))+
   scale_color_manual(values=c("cyan", "green1"))+
   # switch axes and add theme
   coord_flip() + theme_minimal() +
   scale_y_continuous(limits=c(0,1300), breaks = c(0, 400, 800, 1200))+
   # edit the theme
   theme(axis.text.y = element_blank(), axis.text.x = element_text(color="white"),
         axis.title.x=element_text(hjust=1), legend.position="none",
         panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
         panel.grid.major.x = element_line(color="grey50"),
         text = element_text(family="Silom", color="grey70"),
         plot.background = element_rect(fill="grey15"),
         strip.text = element_blank(),
         plot.caption = element_text(color="grey40"),
         plot.title = element_markdown(halign=1, hjust=1, size=18, color="grey40"))+
   # add those labels
   labs(y="Number of times word was said", x=NULL,
        title="<span style='font-size:36pt; color:grey70'>**Star Trek:** Commands</span> <br>
           Most common words used in voice commands by a <span style='color:cyan;'>**computer**</span> or <span style='color:green1;'>**person**</span>",
        caption = "data from SpeechInteraction.org | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("StarTrekVoice_plot.png", width = 10, height = 6, dpi = 400)
