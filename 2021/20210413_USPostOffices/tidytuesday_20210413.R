# US Post offices
# TidyTuesday 2021 week 16
# Rebecca Stevick updated 4/14/2021

# Load libraries -----------------
library(tidyverse)
library(gganimate)
library(magick)

# download font from https://famfonts.com/united-states-postal-service/

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-04-13')
post_offices <- tuesdata$post_offices

# Analysis and plotting ----------
# clean up data
post_offices_clean <- post_offices %>% 
  drop_na(established, longitude, latitude) %>% 
  filter(established >= 1600) %>% # remove random dates
  filter(longitude <= 0) # remove point in Guam

# generate static plot
plot <- post_offices_clean %>% 
  ggplot(aes(y = latitude, x = longitude)) +
  geom_point(aes(color = established), size = 0.4, shape = 19, alpha = 0.2) +
  theme_void() + coord_fixed(1.3) +
  scale_color_gradient(low = "midnightblue", high = "firebrick4") +
  theme(legend.position = "none", plot.title.position = "plot",
        plot.title = element_text(hjust = 1, family = "Postmaster", size = 35, color = "midnightblue"),
        plot.subtitle = element_text(family = "Postmaster", size = 18, color = "grey40", hjust = 1),
        plot.caption = element_text(margin = margin(t = -20, b=0), family = "Silom", size = 14)) +
  labs(title = "You've got mail!",  subtitle = "US post offices established 1639 - 2000",
       caption = "data from Cameron Blevins and Richard W. Helbock  |  plot by @rjstevick for #TidyTuesday")

# animate plot by year
plotanimate <- plot + 
  transition_manual(frames = established, cumulative = TRUE)+
  #  geom_text(aes(x = -100, y = 70, label = "US post offices established in {current_frame}"), family = "Postmaster", size = 18, color = "grey40")
  labs(subtitle = "\n {current_frame}. US post offices established since 1639.")

# render animation
animate(plot = plotanimate, renderer = magick_renderer(),
        nframes = length(unique(post_offices_clean$established)), 
        height = 650, width = 1000, end_pause = 15, res = 100)

# Saving -------------------------
anim_save("USPostOffices_plot.gif")
# save static plot too
ggsave(plot = plot, "USPostOffices_plot.png", bg = "transparent", width = 10, height = 6, dpi = 400)
