# African American Achievements
# TidyTuesday 2020 week 24
# Rebecca Stevick updated 6/9/2020

# Load libraries
library(tidyverse)
library(ggtext)
library(ggrepel)
library(hrbrthemes)

# Load data
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

firsts %>% 
  # select years since 1990
  filter(year>=1990) %>%
  # select females
  filter(gender=="Female African American Firsts") %>%
  # clean up person category
  mutate(person = str_remove_all(person, "\\(.*\\)"), # remove anything in parentheses
         person = str_remove_all(person, "\\(.*"), # remove open parenthesis
         person = str_remove_all(person, "\\[.*\\]")) %>% # remove anything in brackets
  # add a column with name and accomplishments merged
  unite(person, accomplishment, 
        col="name_accomplishment", sep=": ",
        remove=FALSE) %>%
  # plotting time
  ggplot(aes(x=category, y=year, color=category,
             # fix the width of the text
             label=str_wrap(name_accomplishment, 40)))+ 
  # add text to plot that repels (ggrepel)
  geom_label_repel(segment.colour = NA, label.size=0, alpha=0.7, fontface="bold",
                   family=font_an, size=3.2)+
  # move x-axis to the top
  scale_x_discrete(position="top")+
  # make y-axis breaks every 5 years
  scale_y_continuous(breaks = scales::breaks_width(5))+  
  # change the theme
  theme_ipsum()+
  # add colors for each category
  scale_color_manual(values=c("#682C37","#D67B44","#34273B","darkblue","#D95B42","#CEA347","#4E7147"))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(size=2, color="gray75"),
        plot.background = element_rect(fill="gray90", color=NA),
        axis.text = element_text(color="gray20", face="bold"),
        legend.position="none")+
  labs(x=NULL, y="Year",
       caption="Plot by @rjstevick | Source: Wikipedia \"List of African-American firsts\"",
       title="Female African-American \"firsts\" since 1990")


# Saving -----------------------------
ggsave("AfricanAmericanAchievements_plot.png", bg="transparent", width = 15, height = 8, dpi=400)
