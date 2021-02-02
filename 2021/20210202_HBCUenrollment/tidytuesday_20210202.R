# HBCU enrollment
# TidyTuesday 2021 week 6
# Rebecca Stevick updated 2/2/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-02-02')
hs_students <- tuesdata$hs_students
hbcu_all <- tuesdata$hbcu_all

# Analysis and plotting ----------
hs_students %>%
  filter(Total < 2020) %>% 
  select(-contains("Error")) %>% 
  pivot_longer(White1:`Two or more race`) %>% 
  mutate(value=as.numeric(value)) %>% 
  filter(name!="Total - Asian/Pacific Islander") %>% 
  ggplot(aes(Total))+
  geom_col(aes(y=value, fill=name), position="dodge")+
  geom_point(aes(y=`Total, percent of all persons age 25 and over`))+
  geom_line(aes(y=`Total, percent of all persons age 25 and over`))+
  scale_y_continuous(labels=scales::percent_format(scale=1), limits=c(0,100))


  # add those labels
  labs(caption = "data from Data.World | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("HBCUenrollment_plot.png", bg = "transparent", width = 15, height = 8, dpi = 400)
