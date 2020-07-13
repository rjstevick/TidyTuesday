# Astronaut population database
# TidyTuesday 2020 week 29
# Rebecca Stevick updated 7/14/2020

# Load libraries
library(tidyverse)
library(ggchicklet) #remotes::install_github("hrbrmstr/ggchicklet")
library(nationalparkcolors) #devtools::install_github("katiejolly/nationalparkcolors")
library(hrbrthemes)

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-07-14')
astronauts <- tuesdata$astronauts

astronauts %>%
  mutate(nationalityother=fct_lump(f=nationality,
                                   n=10, other_level="Others")) %>%
  group_by(year_of_mission, nationality, nationalityother) %>%
  count() %>% arrange(desc(n)) %>%
  ggplot(aes(x=year_of_mission, y=n, 
             fill=factor(nationalityother,levels=unique(nationalityother)))) +
  geom_chicklet(width=0.7) + 
 # geom_flag()+
  # edit the breaks on x-axis
  scale_x_continuous(breaks=scales::pretty_breaks(n=20))+
  # change color of fill
  scale_fill_manual(values = c(park_palette("SmokyMountains",6),park_palette("Saguaro",6)))+
  # edit the theme
  theme_ipsum()+ theme(legend.position = c(0.2,0.85), legend.direction = "horizontal")+
  # put the legend title on the top
  guides(fill = guide_legend(title.position="top"))+
  # add those labels
  labs(title="Number of astronaut missions each year and their nationalities",
       x="Year of Mission", y="Number of astronauts",
       fill="Astronaut nationality",
       caption = "Data from Corlett, Stavnichuk & Komarova. Life Sciences in Space Research (2020).
       Plot by @rjstevick for #TIdyTuesday")

# Saving -----------------------------
ggsave("Astronauts_plot.png", bg="transparent", width = 11, height = 7, dpi=400)
