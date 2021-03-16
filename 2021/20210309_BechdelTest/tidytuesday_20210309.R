# Bechdel Test
# TidyTuesday 2021 week 11
# Rebecca Stevick updated 3/16/2021

# Load libraries -----------------
library(tidyverse)
library(ggalt)
library(ggtext)
library(patchwork)
library(ggpubr)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-03-09')
movies <- tuesdata$movies

# Set global theme ---------------
theme_set(theme_cleveland() + 
             theme(text = element_text(family = "Rockwell"), plot.title = element_text(size = 24, lineheight = 1.2),
                   legend.position = "none", plot.subtitle = element_markdown(), 
                   plot.caption = element_text(hjust = 0.5, size = 8, color = "grey50")))

# Analysis and plotting ----------

# plot 1 - avg imdb rating of movies that pass/fail per genre
rating <- movies %>% 
   # separate the genre categories into rows
   drop_na(genre) %>% separate_rows(genre, sep = ", ") %>% 
   # calculate the average rating per genre per pass/fail
   group_by(genre, binary) %>% summarise(avgrating = mean(imdb_rating)) %>%
   # put pass/fail into their own columns
   pivot_wider(names_from = binary, values_from = avgrating) %>% 
   # calculate the difference between pass/fail
   mutate(diff = FAIL-PASS) %>% 
   # remove genres with < 30 movies
   filter(genre != "Western" & genre != "War"  & genre != "Documentary"  & genre != "Musical") %>% 
   # start plotting
   ggplot(aes(y = reorder(genre, FAIL), x = PASS, xend = FAIL, color = diff)) +
   # add the dumbbell
   geom_dumbbell(size = 3, colour_x = "aquamarine3", colour_xend = "violetred4") +
   # define gradient for dumbbell bar fill
   scale_color_gradient(low = "grey80", high = "grey40") +
   # add/remove labels
   labs(x = "Average IMDB rating", y = NULL, color = NULL)

# extract order of y-axis values for the second plot
orderedgenre <- ggplot_build(rating)$layout$panel_params[[1]]$y$get_labels()

# plot 2 - number of movies that pass/fail per genre
number <- movies %>% 
   # separate the genre categories into rows
   drop_na(genre) %>% separate_rows(genre, sep = ", ") %>% 
   # count the number of movies per genre per pass/fail
   group_by(genre, binary) %>% count() %>%
   # put pass/fail into their own columns
   pivot_wider(names_from = binary, values_from = n) %>% 
   # calculate the difference between pass/fail
   mutate(diff = FAIL-PASS) %>% 
   # remove genres with < 30 movies
   filter(genre != "Western" & genre != "War"  & genre != "Documentary"  & genre != "Musical") %>% 
   # order the genre categories in the same way as the first ratings graph
   mutate(genre = factor(genre, levels = orderedgenre)) %>% 
   # start plotting
   ggplot(aes(y = genre, x = PASS, xend = FAIL, color = diff)) +
   # add the dumbbell
   geom_dumbbell(size = 3, colour_x = "aquamarine3", colour_xend = "violetred4") +
   # define gradient for dumbbell bar fill
   scale_color_gradient(low = "grey80", high = "grey40") +
   # put the y-axis on the right
   scale_y_discrete(position = "right") +
   # add/remove labels
   labs(x = "Number of movies", y = NULL, color = NULL)

# show both plots together with labels
rating + number +
   # add those labels
   plot_annotation(title = "Gender bias in movies by genre (1970-2013)", 
                   subtitle = "<br> Movies that <span style='color:violetred4;'>**fail**</span> the Bechdel test usually get higher IMDB ratings, except in the Animation and Family genres.  
                   But, there are more movies that <span style='color:aquamarine3;'>**pass**</span> the Bechdel test in the Music, Romance, Horror, and Family genres.  
                   In the Action genre, there are 2.4 movies that <span style='color:violetred4;'>**fail**</span> the Bechdel test for every 1 movie that <span style='color:aquamarine3;'>**passes**</span>. <br>",
                   caption = "data from FiveThirtyEight | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("BechdelTest_plot.png", bg = "transparent", width = 10, height = 6, dpi = 400)
