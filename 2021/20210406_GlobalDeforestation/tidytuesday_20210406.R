# Deforestation in Brazil
# TidyTuesday 2021 week 15
# Rebecca Stevick updated 4/6/2021

# Load libraries -----------------
library(tidyverse)
library(ggstream) # remotes::install_github("davidsjoberg/ggstream")
library(nationalparkcolors)
library(scales)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-04-06')
brazil_loss <- tuesdata$brazil_loss

# Analysis and plotting ----------
brazil_loss %>% 
   pivot_longer(commercial_crops:small_scale_clearing) %>% 
   mutate(nameother = forcats::fct_lump_n(name, w = value, n = 5)) %>% 
   mutate(nameother = str_to_sentence(str_replace_all(nameother, "_"," "))) %>% 
   group_by(year, nameother) %>% summarise(sumvalue = sum(value)) %>% 
   ggplot(aes(x = year, y = sumvalue, fill = reorder(nameother, desc(sumvalue)))) +
   geom_stream(color = "white", alpha = 0.7, type = "ridge", bw = 0.9) +
   geom_stream_label(aes(label = nameother), type = "ridge", family = "Krungthep") +
   scale_fill_manual(values = c("#092215", rev(park_palette("Acadia")))) +
   scale_y_continuous(labels = label_number(suffix = " million ha", scale = 1e-6, accuracy = 1), expand = c(0,0)) +
   scale_x_continuous(breaks = scales::breaks_pretty(), expand = c(0,0)) +
   theme_minimal() +
   theme(legend.position = "none", text = element_text(family = "American Typewriter"), 
         plot.title = element_text(size = 22, family = "Krungthep"), axis.ticks = element_line(size = 0.2),
         panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
   labs(title = "Causes of forest loss in Brazil",
        subtitle = "Annual forest loss due to deforestion or degradation from 2001-2013, measured in hectares",
        x = NULL, y = NULL, caption = "data from Our World in Data  |  plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("GlobalDeforestation_plot.png", bg = "transparent", width = 10, height = 6, dpi = 400)
