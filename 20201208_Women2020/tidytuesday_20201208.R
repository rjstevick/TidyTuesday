# Women of 2020 poster
# TidyTuesday 2020 week 50
# Rebecca Stevick updated 12/08/2020

# Load libraries -----------------
library(tidyverse)
library(ggimage)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-12-08')

# Analysis and plotting ----------
tuesdata$women %>%
  # make a  grid guide for the points, based on 8 rows/columns
  mutate(num = row_number(), x_pos = (num - 1) %/% 10, y_pos = 10 - (num - 1) %% 10 - 1) %>%
  # time to plot
  ggplot()+ 
  # add background rectangles based on woman's category
  geom_rect(aes(xmin = x_pos-0.5, xmax = x_pos+0.5, ymin = y_pos-0.5, ymax = y_pos+0.5, fill=category), color="#FBEAD6", lwd=2) +
  # add photo of woman
  geom_image(aes(x = x_pos, y = y_pos+0.1, image = img), size = 0.05, by = "width") +
  # add woman's names
  geom_text(aes(x = x_pos, y = y_pos-0.3, label = str_wrap(name, width=15)), size = 4, hjust=0.5, lineheight=0.8) +
  # make sure the photos don't warp and remove extra white space
  scale_size_identity() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
  # change background colors
  scale_fill_manual(values=c("#FBEAD6", "#83CDC0", "#D3A3A1", "#A79CA5","#466D53"),
                    guide = guide_legend(keyheight = unit(12, units = "mm"), keywidth = unit(12, units = "mm"))) +
  # change theme
  theme_void() +
  # edit theme
  theme(text = element_text(family="Amaranth", size=22), plot.title = element_text(size=40, hjust=0.5),
        legend.position = "bottom", legend.justification = c(0,0)) +
  # add those labels
  labs(title="Women of 2020", fill=NULL, x=NULL, y=NULL,
       caption = "data from the BBC | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("Women2020_plot.png", bg = "transparent", width = 15, height = 15, dpi = 400)
