# HBCU enrollment
# TidyTuesday 2021 week 6
# Rebecca Stevick updated 2/3/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-02-02')
hs_students <- tuesdata$hs_students

# Analysis and plotting ----------
hs_students %>%
  # remove random erroneous data in the Total column (which is actually year)
  filter(Total < 2020) %>%
  # remove the columns with standard error
  select(-contains("Error")) %>%
  # put all percentages by race/ethnicity into a single column
  pivot_longer(White1:`Two or more race`) %>%
  # remove this total category
  filter(name != "Total - Asian/Pacific Islander") %>%
  # add the education percentage as a numeric value
  mutate(percent = as.numeric(value)) %>%
  # define order of race/ethnicity variable
  mutate(name = factor(name, levels=c("White1", "Asian/Pacific Islander - Pacific Islander",
                                      "Two or more race", "Asian/Pacific Islander - Asian", "Black1",
                                      "American Indian/\r\nAlaska Native", "Hispanic"))) %>%
  # start plotting - year on the x-axis, education rate on the y-axis, color by race/ethnicity
  ggplot(aes(x = Total, y = percent, fill = name))+
  # add area with dodged position so they all plot in front of each other
  geom_area(position = position_dodge(width = 0), alpha = 0.7, color = "white")+
  # add color scheme and better labels for the fill colors
  scale_fill_viridis_d(option = "A", labels = c("White", "Pacific Islander", "Two or more races",
                                                "Asian", "Black","American Indian/Alaska Native", "Hispanic"))+
  # add percent scale to the y-axis
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,100))+
  # add global theme
  theme_ft_rc()+
  # edit theme
  theme(legend.position = c(0.2,0.73), legend.text = element_text(color="gray90"),
        plot.caption = element_text(color="white"), plot.title = element_text(size = 26, family = "Education Pencil"),
        panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line(color="gray70"))+
   # add those labels
  labs(title = "The race for education",
       subtitle = "Inequities in high school education rate have barely improved since 1940",
       x = NULL, y = "Percent of persons age 25 and over with a high school degree", fill = NULL,
       caption = "data from Data.World | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("HBCUenrollment_plot.png", bg = "transparent", width = 10, height = 6, dpi = 400)
