# Makeup Shades
# TidyTuesday 2021 week 14
# Rebecca Stevick updated 3/30/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)
library(patchwork)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-03-30')
allShades <- tuesdata$allShades


# Analysis and plotting ----------

# Make overall palette with one square for each foundation color available
paletteplot <- allShades %>%
   # remove the random blue shade
   filter(hex != "#4460F3") %>%
   # count up number of products per hex, then make a row for each
   group_by(hex, lightness) %>% count() %>% uncount(n) %>% ungroup() %>%
   # arrange by lightness of the hue
   arrange(desc(lightness)) %>%
   # make a waffle guide for the tiles, based on 62 rows
   mutate(num = row_number(), x_pos = (num - 1) %/% 62, y_pos = 62 - (num - 1) %% 62 - 1) %>%
   # time to plot
   ggplot(aes(x = x_pos, y = y_pos, fill = hex)) +
   # add tiles
   geom_tile(color = "white", lwd = 0.6) +
   # color by hex code and remove extra space on x-axis
   scale_fill_identity() + scale_x_continuous(expand = c(0, 0)) +
   # change global theme, remove legend, edit caption text
   theme_void() + theme(legend.position = "none", plot.caption = element_text(size = 14, family = "Avenir")) +
   # add caption
   labs(caption = "data from The Pudding  |  plot by @rjstevick for #TidyTuesday")


# Make header palette with randomly selected (average/representative) palette
header <- allShades %>% 
   # select the darkest shade
   arrange(desc(lightness)) %>% slice_head(n = 1) %>%
   # join with the lighest shade
   full_join(allShades %>% arrange(desc(lightness)) %>% slice_tail(n = 1)) %>%
   # join with 1% of randomly selected shades
   full_join(allShades %>% filter(hex != "#4460F3") %>% sample_frac(size = 0.01)) %>%
   # pick only hex and lightness columns, and arrange based on lightness of the hue
   select(hex, lightness) %>% arrange(desc(lightness)) %>% 
   mutate(num = row_number()) %>% 
   # time to plot
   ggplot() + 
   # add tiles sorted by lightness
   geom_tile(aes(x = num, y = 1, fill = hex), color = "black", lwd = 1.2) +
   # add hex code labels
   geom_text(aes(x = num, y = 1, label = hex, color=rev(hex)), angle=90)+
   # add title with "shadow"
   annotate(geom = "text", x = 35.1, y = 0.98, label = "beauty bias palette", family = "Avenir Black", size = 17, color = "black") +
   annotate(geom = "text", x = 35, y = 1, label = "beauty bias palette", family = "Avenir Black", size = 17, color = "white") +
   # color by hex code and remove extra white space on the x-axis
   scale_fill_identity() + scale_color_identity() + scale_x_discrete(expand = c(0, 0)) +
   # change global theme, edit caption/subtitle text
   theme_void() + theme(plot.caption = element_text(hjust = 0.5, family = "Avenir", size = 14)) +
   # add subtitle/caption
   labs(caption = "Representation of 6,815 foundation shades available from Sephora or Ulta US. Lighter shades have more diversity and availability.")

# plot header over palette and adjust heights of panels
header/paletteplot+plot_layout(heights=c(1,6))

# Saving -------------------------
ggsave("MakeupShades_plot.png", width = 14, height = 8, dpi = 400)
