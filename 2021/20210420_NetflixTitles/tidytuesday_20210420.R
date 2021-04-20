# Netflix Shows
# TidyTuesday 2021 week 17
# Rebecca Stevick updated 4/20/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)
library(ggimage)
library(ggtext)
# download font from https://fontmeme.com/fonts/bebas-neue-font/

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
# store cartoon TV image as a dataframe
images <- tibble(image=c("https://i.pinimg.com/originals/87/9f/35/879f354dfe3834de15865303aa22995a.png"))

# Analysis and plotting ----------
tuesdata$netflix %>% 
   # select only TV shows
   filter(type=="TV Show") %>% 
   # separate the number of seasons column into 2, since there's some text
   separate(duration, into = c("seasons", "duration")) %>% 
   # make a row for each category listed per show, then remove broad category
   separate_rows(listed_in, sep = ", ") %>% filter(listed_in!="TV Shows") %>%
   # make the number of seasons a numeric variable, and clean up some of the names
   mutate(seasons = as.numeric(seasons),
          listed_in = recode(listed_in, "Spanish-Language TV Shows" = "Spanish-Language",
                             "Stand-Up Comedy & Talk Shows" = "Stand-Up Comedy & Talk",
                             "International TV Shows" = "International TV")) %>% 
   # start plotting, make a panel for each category in 6 columns
   ggplot(aes(y=0, x=0)) + facet_wrap(.~listed_in, ncol = 6) +
   # add TV pngs to each panel background
   geom_image(data = images, aes(x=0.056, y=0.053, image = image), size = 1.3, by="height")+
   # add black background to each TV screen
   annotate("rect", xmin=-0.38, ymin=-0.29, xmax=0.37, ymax=0.28, fill = "black", color = "black")+
   # add points jittered
   geom_jitter(aes(y = -0.05, size = seasons, color = seasons), shape = 20, width = 0.35, height = 0.2) +
   # add titles of each category at the top of the TV screens
   stat_summary(aes(color = seasons, x = 0, y = 0.23, label = listed_in), fun = "mean", geom = "text", 
                size = 4.4, family = "Bebas Neue", color = "red2", lineheight = 0.8) +
   # define color scheme for points
   scale_color_gradient(low="grey65", high="white") +
   # define size scheme for points
   scale_size_continuous(range = c(0.5, 3)) +
   # define limits for x and y-axis so we can fit the TV in the background
   scale_x_continuous(limits = c(-0.4, 0.4)) + scale_y_continuous(limits = c(-0.4, 0.4)) +
   # add text/caption at bottom right, set to the right of the TV Thrillers panel
   geom_text(data = data.frame(listed_in = c("TV Thrillers")), x = 3.6, y = -0.2, color = "white", 
             size = 2.8, hjust = 1, family = "Monaco", 
             label = "Each point represents one TV show per category.
             The size of each point corresponds to the\n number of seasons per TV show (1-16).\n
             data from Shivam Bansal via Kaggle | plot by @rjstevick for #TidyTuesday") +
   # set global theme and make sure the plot panel isn't cut off
   theme_ft_rc() + coord_cartesian(clip = 'off') +
   # edit the theme
   theme(legend.position = "none", panel.spacing = unit(1.2, "lines"), strip.text = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        plot.title = element_markdown(hjust = 0.5, family = "Bebas Neue", size = 46, color = "white")) +
   # add the title
   labs(title = "What's on <span style='color:red;'>**Netflix**</span> tonight?", x = NULL, y = NULL)

# Saving -------------------------
ggsave("NetflixTitles_plot.png", width = 12, height = 7.5, dpi = 400)
