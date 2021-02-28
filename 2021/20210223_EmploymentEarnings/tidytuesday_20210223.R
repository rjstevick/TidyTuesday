# Employment and Earnings
# TidyTuesday 2021 week 9
# Rebecca Stevick updated 2/28/2021

# Load libraries -----------------
library(tidyverse)
library(ggalt)
library(ggtext)
library(gganimate)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-02-23')
employed <- tuesdata$employed

# Analysis and plotting ----------
employed %>%
  filter(race_gender!="TOTAL") %>% # filter(year==2020) %>%
  mutate(gender=case_when(race_gender=="Men" ~ "Men",
                        race_gender=="Women" ~ "Women",
                        TRUE ~ "NA")) %>% filter(gender!="NA") %>%
  drop_na() %>%
  mutate(industry = recode(industry, "Mining, quarrying, and\r\noil and gas extraction" = "Mining, quarrying, and oil and gas extraction")) %>% 
  group_by(gender, industry, year) %>% summarise(employed = sum(employ_n)) %>% 
  group_by(industry, year) %>% mutate(percent = employed / sum(employed)) %>%
  select(-c(employed)) %>% 
  pivot_wider(names_from = gender, values_from = percent) %>%  mutate(diff = Men-Women) -> employededit

# start plotting
ggplot(employededit, aes(y = reorder(industry,diff), x = Women, xend = Men))+
  geom_dumbbell(size = 4, colour_x = "#7669a8", colour_xend = "#509e9d", color = "grey80", alpha = 0.7)+
  geom_text(aes(label = industry, color = diff), x = 0.5, family = "Copperplate")+
  scale_color_gradient2(high = "#509e9d", mid = "black", low = "#7669a8", midpoint = 0)+
  scale_x_continuous(labels = scales::label_percent(), limits = c(0,1))+
  theme_void()+
  theme(text = element_text(family = "Helvetica", color = "grey30"),
        legend.position = "none", axis.text.x = element_text(inherit.blank = FALSE),
        panel.grid.major.x = element_line(inherit.blank = FALSE, color = "grey90"), 
        plot.background = element_blank(),
        plot.title = element_markdown(hjust = 0.5, lineheight = 1.1, color = "grey30", family = "Copperplate", size=24),
        plot.subtitle = element_markdown(hjust = 0.5, lineheight = 0.5, margin = margin(4,0,10,0)), 
        plot.caption = element_text(hjust = 0.5,  margin = margin(10,0,0,0))) -> plot

#animate based on year
plotanimate <- plot + transition_manual(frames = year, cumulative = FALSE)+
  # add those labels
  labs(title = "Do industries employ more <span style='color:#509e9d;'>**men**</span> or <span style='color:#7669a8;'>**women**</span>?",
       subtitle = "Each point indicates the percentage of each gender per industry in {current_frame}",
       caption = "data from BLS | plot by @rjstevick for #TidyTuesday")

# render animation
animate(plot = plotanimate, nframes = length(unique(employededit$year)), 
        fps = 1, height = 500, width = 1000, res = 100)

# Saving -------------------------
anim_save("EmploymentEarnings_plot.gif")
