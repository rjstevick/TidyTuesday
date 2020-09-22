# Himalayan Climbers
# TidyTuesday 2020 week 39
# Rebecca Stevick updated 9/22/2020

# Load libraries ---------------
library(tidyverse)
library(ggridges)
library(hrbrthemes)

# Load data --------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-22')
peaks <- tuesdata$peaks

# Analysis and plotting ------

peaks %>% 
  # select only climbed peaks
  filter(climbing_status=="Climbed") %>% 
  # separate out the countries in each expedition
  separate_rows(first_ascent_country, sep=", ") %>% 
  # fix some country names
  mutate(first_ascent_country=recode(first_ascent_country, 
                  "US"="USA", "Netherands"="Netherlands",
                  "Inida"="India", "W Germany"="Germany")) %>% 
  
  # count up ascents per country for cleaner plotting
  group_by(first_ascent_country) %>% add_count() %>% ungroup() %>% 
  # select countries with more than 2 ascents
  filter(first_ascent_year>1900, n>2) %>% 
  # time to plot!
  ggplot(aes(x=first_ascent_year, 
             y=reorder(first_ascent_country,n),
             fill=stat(x)))+
  # add ridges
  geom_density_ridges_gradient(alpha=0.8, scale=2,rel_min_height = 0.005)+
  # add points for each first ascent
  geom_jitter(alpha=0.4, shape=20, width=0, color="steelblue4")+
  # set limits on x-axis and remove extra space
  scale_x_continuous(limits=c(1900, 2030), expand=c(0,0))+
  # edit fill color
  scale_fill_gradient(low="aliceblue", high="steelblue4")+
  # edit theme
  theme_ipsum()+
  theme(legend.position="none", panel.grid.major.y = element_blank(),
        title=element_text(color="#254661"))+
  # add those labels
  labs(x=NULL, y=NULL, 
       title="Nepalese climbers have participated in the most first ascents of the Himalayas",
       subtitle="Timeline of first ascents of Himalayan Peaks by country",
       caption="All countries with more than 2 first ascents are shown.
       data from The Himalayan Database/Elizabeth Hawley | plot by @rjstevick for #TidyTuesday")

# Saving -----------------------
ggsave("HimalayanClimbers_plot.png", bg="transparent", width = 12, height = 6.5, dpi = 400)
