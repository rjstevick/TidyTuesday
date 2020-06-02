# Marble races - top 3 ggbump
# TidyTuesday 2020 week 23
# Rebecca Stevick updated 6/2/2020

# Load libraries ---------------------

library(tidyverse)
library(ggbump)

# Load data
marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

# let's get piping!
marbles %>%
  # group data by marble race
  group_by(race) %>%
  # make a new rank column per race group
  mutate(rank = rank(time_s, ties.method = "random")) %>%
  # select only the top 3 marbles
  filter(rank <= 3) %>%
  # plotting time!
  ggplot(aes(x=race,
             # make the y-axis a factor and flip it
             y=reorder(as.factor(rank),-rank),
             # need to define group for the colors
             color=team_name, group=team_name))+
  # add ggbump lines
  geom_bump(size = 2, alpha=0.8)+
  # remove extra white space
  scale_y_discrete(expand=c(0.1,0.1))+
  # add points since they look like marbles!
  geom_point(size = 6, alpha=0.8) +
  # remove the background lines
  cowplot::theme_minimal_grid(font_size = 14, line_size = 0) +
  # and edit the theme
  theme(legend.position="bottom",
        panel.grid.major = element_blank(),
        text = element_text(size=18, color="grey30"),
        axis.text.x = element_text(color="grey10", face="bold"),
        plot.caption = element_text(size=12, color="grey50"))+
  # define the colors
  scale_color_manual(values = c("#d9498b","#61b855","#b452bc","#bab141","#6f67d1","#d57731","#618aca",
                                "#c9423c","#51b79f","#9b476c","#5f813d","#c585c8","#ac8044","#d67776"))+
  # add those labels
  labs(x="Race", y="Team Ranking", color=NULL,
       caption="Plot by @rjstevick | Source: Jelle's Marble Runs",
       title="Every team wins all the time!",
       subtitle="Top 3 teams at each race")


 # Saving -----------------------------

ggsave("MarbleRaces_plot.png", bg="transparent", width = 12, height = 6.5, dpi=400)
