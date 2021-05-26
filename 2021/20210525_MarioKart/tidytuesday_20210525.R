# Mario Kart World Records
# On which track does the shortcut save the most time? 
# TidyTuesday 2021 week 22
# Rebecca Stevick updated 5/26/2021

# Load libraries -----------------
library(tidyverse)
library(ggalt)
library(hrbrthemes)
library(ggtext)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-05-25')
records <- tuesdata$records

# Analysis and plotting ----------
records %>%
   # select only 3-lap times
   filter(type == "Three Lap") %>% 
   # calculate the average time per track per shortcut/not
   group_by(shortcut, track) %>% summarise(meantime = mean(time)) %>% 
   # put shortcut or not into their own columns
   pivot_wider(names_from = shortcut, values_from = meantime) %>% 
   # calculate the difference between shortcut/not
   mutate(diff = Yes - No) %>% drop_na(diff) %>% 
   # start plotting
   ggplot(aes(y = reorder(track, -diff), x = Yes, xend = No, color = diff)) +
   # add the dumbell
   geom_dumbbell(size = 5, colour_xend = "red3", colour_x = "dodgerblue") +
   # add vertical start and finish lines
   geom_vline(aes(xintercept = 0), color = "white") + geom_vline(aes(xintercept = 400), color = "white") +
   # define gradient for dumbbell bar fill
   scale_color_gradient(low = "grey90", high = "grey30") +
   # change global theme
   theme_ft_rc() + 
   # edit theme
   theme(plot.title = element_text(size = 24), plot.subtitle = element_markdown(lineheight = 1.1), 
         panel.grid.major.y = element_line(linetype = "dashed", color = "grey90"),
         axis.text.y = element_text(color = "white", size = 14), axis.text.x = element_text(color = "white", size = 14), 
         axis.title.x = element_text(color = "grey90", size=12), legend.position = "none") +
   # add those labels
   labs(title = "The Mario Kart shortcut always saves time", 
        subtitle = "World records that <span style='color:dodgerblue;'>**take the shortcut**</span> are 
        always <span style='color:white;'>faster</span> than records that <span style='color:red3;'>**take the long road**</span>.
        <br>Shortcuts save an average of <span style='color:white;'>58 seconds</span> on a 3-lap course. 
        The most time saved by <span style='color:dodgerblue;'>**taking the shortcut**</span> 
        is on the <span style='color:white;'>Wario Stadium track</span>.",
        fill = NULL, color = NULL, y = NULL, x = "Average record time for a 3-lap course (seconds)",
        caption = "data from Mario Kart World Records | plot by @rjstevick for #TidyTuesday")
   
# Saving -------------------------
ggsave("MarioKart_plot.png", bg = "transparent", width = 12, height = 7, dpi = 400)
