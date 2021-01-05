# BLack in Data week table
# TidyTuesday 2020 week 47
# Rebecca Stevick updated 11/17/2020

# Load libraries -----------------
library(tidyverse)
library(gt)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-11-17')

# Analysis and plotting ----------
table <- tuesdata$black_in_data %>% 
  gt() %>%
  fmt_date(columns = vars(date), date_style = 2) %>% 
  fmt(columns = 'link',
      fns = function(myurl,mytext=myurl) {paste('<a href="',myurl,'">',mytext,'</a>')}) %>% 
  opt_table_font(font = list(google_font("Space Mono"), default_fonts())) %>% 
  tab_options(table.background.color="azure",
              column_labels.background.color = "black",
              table.font.size = px(12),
              column_labels.font.size = px(20),
              heading.background.color = "white",
              heading.align = "left",
              heading.title.font.size = px(28)) %>% 
  tab_header(title = "#BlackInDataWeek events -- November 16-21, 2020") %>% 
  tab_source_note(source_note = "data from #BlackInDataWeek | table by @rjstevick for #TidyTuesday")
table

# Saving -------------------------
table %>% gtsave("BlackInData_plot.png")
