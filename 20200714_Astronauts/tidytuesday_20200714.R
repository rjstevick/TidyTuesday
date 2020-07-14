# Astronaut population database
# TidyTuesday 2020 week 29
# Rebecca Stevick updated 7/14/2020

# Load libraries
library(tidyverse)
library(ggchicklet) #devtools::install_github("hrbrmstr/ggchicklet")
library(nationalparkcolors) #devtools::install_github("katiejolly/nationalparkcolors")
library(hrbrthemes)

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-07-14')
astronauts <- tuesdata$astronauts

# Goal: number of astronauts per year and their nationalities. 
# And which spacecraft did they take?

# Let's get piping!
astronauts %>%
  # select the top 2 countries (US and Russia) and lump all others into "Others"
  mutate(nationalityother=fct_lump(f=nationality, n=2, other_level="Others")) %>%
  # make new column with the cleaned spacecraft name. Extract the word before a " " or a "-"in the ascend_shuttle column
  mutate(spacecraft=str_extract(ascend_shuttle, "[^ |-]+")) %>%
  # remove any mission numbers in the spacecraft column
  mutate(spacecraft=str_extract(spacecraft, "[:alpha:]+")) %>%
  # fix some of the names
  mutate(spacecraft=recode(spacecraft, "gemini"="Gemini", "soyuz"="Soyuz", "apollo"="Apollo",
                           "ASTP"="Apollo-Soyuz", "STS"="Space Shuttle", "MA"="Mercury-Atlas")) %>%
  # count number of astronauts per year and nationality and arrange
  group_by(year_of_mission, nationalityother, spacecraft) %>% count() %>% arrange(desc(n)) %>%
  # plot year on x-axis and number of missions on y-axis
  ggplot(aes(x=year_of_mission, y=n, 
             # fill by nationality, in order of abundance
             fill=factor(nationalityother,levels=unique(nationalityother)),
             # outline color by spacecraft
             color=spacecraft))+
  # add rounded bar graph (chicklets)
  geom_chicklet(width=0.7, lwd=1)+
  # edit the breaks on x-axis
  scale_x_continuous(breaks=scales::pretty_breaks(n=20))+
  # change fill colors using national park palettes
  scale_fill_manual(values=c(park_palette("Saguaro", 3)))+
  # change outline colors
  scale_color_manual(values=rev(RColorBrewer::brewer.pal(n = 11, name = "Set3")))+ 
  # edit the theme
  theme_ipsum()+ theme(legend.position = c(0.2,0.78), legend.direction = "horizontal", 
                       plot.background = element_rect(fill="gray60", color="transparent"), 
                       panel.grid.minor = element_blank())+
  # put the legend titles on top
  guides(fill=guide_legend(position=1, title.position="top"),
         # the chicklet outlines were tricky... this is the only way I could get colors to show up
         color=guide_legend(title.position="top", override.aes = list(fill=rev(c("white",RColorBrewer::brewer.pal(n = 11, name = "Set3"))))))+
  # add those labels
  labs(title= "Space Race! Astronaut missions peaked in 1985.",
       subtitle="Number of astronaut missions each year are colored by nationality. Outline color indicates which spacecraft used for ascent. 948/1277 missions were U.S. astronauts in the Space Shuttle.",
       x="Year of Mission", y="Number of astronauts", fill="Astronaut Nationality (fill)", color="Ascent Spacecraft Used (outline)", 
       caption = "Data from Corlett, Stavnichuk & Komarova. Life Sciences in Space Research (2020).
       Plot by @rjstevick for #TidyTuesday")

# Saving -----------------------------
ggsave("Astronauts_plot.png", width = 14, height = 7, dpi=400)
