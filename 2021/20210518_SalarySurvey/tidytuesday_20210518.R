# Ask a Manager Salary Survey
# TidyTuesday 2021 week 21
# Rebecca Stevick updated 5/27/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-05-18')
survey <- tuesdata$survey

# Analysis and plotting ----------
survey %>% 
   # remove data without gender or with likely erroneous salaries
   drop_na(gender) %>% filter(annual_salary<3e7) %>% 
   # fix and reorder data
   mutate(gender = recode(gender, "Prefer not to answer" = "Other or prefer not to answer"),
          gender = factor(gender, levels = c("Man", "Woman", "Non-binary", "Other or prefer not to answer")),
          years = factor(overall_years_of_professional_experience,
                         levels = c("1 year or less", "2 - 4 years", "5-7 years", "8 - 10 years",   
                                    "11 - 20 years", "21 - 30 years", "31 - 40 years", "41 years or more"))) %>% 
   # start plotting
   ggplot(aes(x = years, y = annual_salary, color = gender)) +
   stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 1)) +
   stat_summary(fun = "mean", geom = "segment", 
                aes(yend = 0, xend = years), position = position_dodge(width = 1)) +
   scale_y_continuous(labels = scales::label_dollar()) +
   scale_color_manual(values = PNWColors::pnw_palette("Cascades", n=4)) +
   # set and edit theme
   theme_ipsum() + theme(legend.position = c(0.75, 1.05), legend.direction = "horizontal") +
   # add those labels
   labs(title = "Annual salaries increase with years of experience",
        subtitle = "But, men always make more money than women at all stages",
        y = "Average annual salary", x = NULL, color = NULL,
        caption = "data from Ask a Manager Salary Survey | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("SalarySurvey_plot.png", bg = "transparent", width = 10, height = 5, dpi = 400)
