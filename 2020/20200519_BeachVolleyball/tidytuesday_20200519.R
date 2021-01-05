# Beach volleyball
# TidyTuesday 2020 week 21
# RJS updated 5/19/2020

# Load libraries
library(tidyverse)
sessionInfo()
theme_set(theme_light())

# Load data
vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

# Data formatting & analysis % plotting in one step!! 
vb_matches %>%
  # put all player countries into one column named "player_country"
  gather(c(w_p1_country, w_p2_country, 
           l_p1_country, l_p2_country),
         key="player", 
         value="player_country") %>%
  # group by the country columns
  group_by(country, player_country)  %>%
  # count number of combos per country:player_country
  count() %>% 
  # clean up variable
  drop_na() %>% ungroup() %>%
  # define scale so the outlier isn't too obnoxious
  mutate(ncolors=cut(n, breaks=c(0,10,25,50,75,100,125,150,175,200,1000,25000,50000,max(n)),
                      labels=c(10,25,50,75,100,125,150,175,200,1000,25000,50000,max(n)))) %>%
  # Plotting
  ggplot(aes(y=reorder(country,-n), 
           x=reorder(player_country,-n),
           size=ncolors, 
           color=ncolors))+
  geom_point(alpha=0.8)+
  scale_colour_viridis_d(option = "plasma") +
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.text = element_text(size=8),
        legend.position="right",
        plot.background = element_rect(fill="bisque"),
        legend.background = element_blank(),panel.background = element_blank(),legend.key=element_blank())+
  labs(x="Player countries", y="Match location", 
       color="Number of \ncombinations",size="Number of \ncombinations",
       caption = "Plot by @rjstevick \n Source: BigTimeStats",
       title = "Where do beach volleyball players come from and where do they play?",
       subtitle = "The most common combo is from the USA, playing in the USA")

# Saving -----------------------------
ggsave("BeachVolleyball_plot.png", bg="transparent", width = 12, height = 6.5, dpi=400)
