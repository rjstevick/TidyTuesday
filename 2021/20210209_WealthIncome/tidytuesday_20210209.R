# Wealth and income - lifetime earnings by race/gender
# TidyTuesday 2021 week 7
# Rebecca Stevick updated 2/9/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-02-09')
# use lifetime earn data, but many more included in tuesdata
lifetime_earn <- tuesdata$lifetime_earn
# recreate a version of figure 5 shown here https://apps.urban.org/features/wealth-inequality-charts/

# Analysis and plotting ----------
lifetime_earn %>%
  # add a column with the fomatted earning values
  mutate(lifetime_earn_format = paste("$",round(lifetime_earn/1000000,1)," million", sep = ""),
         # edit the hispanic category name
         race = recode(race, "Hispanic any race"="Hispanic")) %>%
  # start plotting
  ggplot(aes(x = race, y = lifetime_earn, fill = gender))+
  # add barplot, with bars dodged by gender
  geom_col(position = position_dodge(width = 0.95))+
  # annotate the top of each bar with its value
  geom_text(aes(label = lifetime_earn_format, y = lifetime_earn+100000), 
            position = position_dodge(width = 0.95), family = "Arial Narrow", color = "grey40")+
  # add a horizontal line at y=0
  geom_abline()+
  # add dollar signs to the y-axis scale
  scale_y_continuous(labels = scales::label_dollar())+
  # edit the fill colors
  scale_fill_manual(values = c("midnightblue","plum4"))+
  # change the global theme
  theme_ipsum()+
  # edit the theme
  theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(face = "bold", size = 16, color = "black"),
        legend.position = c(0.15,0.93), legend.direction = "horizontal", legend.text = element_text(size = 14, face = "bold"),
        plot.caption = element_text(color = "grey40"), axis.text.y = element_text(color = "grey40"),
        plot.title = element_text(size = 24), plot.subtitle = element_text(color = "grey40"))+
  # add those labels
  labs(title = "Inequities add up in lifetime earnings",
       subtitle = "Average lifetime earnings by race/ethnicity and gender at age 58-62, for people born 1950-54. Data in 2015 $USD.",
       x = NULL, y = NULL, fill = NULL,
       caption = "data from Melissa Favreault, Urban Institute's tabulations from the 2008 Survey of Income and Program \nplot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("WealthIncome_plot.png", bg = "transparent", width = 10, height = 5.8, dpi = 400)
