# Chopped TV Show network
# TidyTuesday 2020 week 35
# Rebecca Stevick updated 8/27/2020

# Load libraries
library(tidyverse)
library(hrbrthemes)
library(tidygraph)
library(ggraph)
library(nationalparkcolors)
library(patchwork)

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-08-25')
chopped <- tuesdata$chopped

# My first attempt at a network in awhile!
# Unfortunately, the ingredients aren't really connected and this got out of hand pretty quickly...
# not the best code, but it gets the job done.

# List of most common dessert ingredients (occuring in >9 episodes)
topingredients <- chopped %>%
  select(dessert, series_episode) %>%
  separate_rows(dessert, sep=", ") %>%
  group_by(dessert) %>% count() %>% ungroup() %>%
  filter(n>=9) %>% mutate(topingredient=dessert)
# Make bar/lollipop plot of ingredients
barplot<-ggplot(topingredients, aes(x = n, y = reorder(dessert,-n))) +
  geom_segment(aes(x = 0, y = reorder(dessert,-n),xend = n, yend = reorder(dessert,-n)),
               color = "chocolate4",alpha = 0.6, lwd = 1.5)+
  geom_point(aes(color=dessert), size = 4, alpha = 0.9)+
  scale_x_continuous(expand = c(0,0.2), breaks = scales::breaks_pretty(n=5))+
  scale_color_manual(name=NULL, values=c("purple4","navyblue","burlywood3","aquamarine4",
                                         "cadetblue2","coral2","darkgoldenrod2","darkred"))+
  theme_ipsum()+ theme(panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_text(size=12), legend.position="none",
        plot.margin =  unit(c(0,0,0,0), "cm"))+
  labs(x="Number of episodes", y=NULL)
# and I should've just stopped here :)


# Network of which ingredients are paired with the most common ingredients
# Make full table of all episodes with the top ingredients included
full_table<-chopped %>%
  select(series_episode, dessert) %>%
  separate(dessert, sep=", ", into=c("a1", "a2", "a3", "a4")) %>%
  filter(a1 %in% topingredients$dessert | a2 %in% topingredients$dessert |
           a3 %in% topingredients$dessert | a4 %in% topingredients$dessert)

# Make pairs of all ingredients for each episode
comboingredients<-
  full_table[1:3] %>%
  full_join(full_table[c(1,2,4)], by=c("a1"="a1","a2"="a3", "series_episode"="series_episode")) %>%
  full_join(full_table[c(1,3,4)], by=c("a1"="a2","a2"="a3", "series_episode"="series_episode")) %>%
  full_join(full_table[c(1,2,5)], by=c("a1"="a1","a2"="a4", "series_episode"="series_episode")) %>%
  full_join(full_table[c(1,3,5)], by=c("a1"="a2","a2"="a4", "series_episode"="series_episode")) %>%
  full_join(full_table[c(1,4,5)], by=c("a1"="a3","a2"="a4", "series_episode"="series_episode")) %>%
  drop_na(a1, a2) %>% group_by(G1 = pmin(a1, a2), G2 = pmax(a1, a2)) %>% 
  add_tally(name="comboN") %>% mutate(comboN=as.character(comboN)) %>% arrange(desc(comboN))

# Get the top ingredient in the first column and build graph object
graphframe<-
  comboingredients %>%
  full_join(topingredients, by=c("G1"="dessert")) %>%
  full_join(topingredients, by=c("G2"="dessert")) %>%
  mutate(n=coalesce(n.x, n.y)) %>%
  mutate(topingredient=coalesce(topingredient.x, topingredient.y)) %>%
  mutate(pairedingredient=case_when(is.na(topingredient.x) ~ G1,
                                    is.na(topingredient.y) ~ G2,
                                    TRUE ~ "other")) %>%
  drop_na(topingredient) %>% ungroup() %>%
  select(topingredient, pairedingredient, topingredient, series_episode) %>%
  drop_na() %>% mutate(topingred=topingredient) %>%
  as_tbl_graph() %>% activate(nodes) %>% 
  mutate(Popularity = centrality_power()) %>% mutate(Popularitycut=cut_interval(Popularity, n = 2))

# Plot the network
graph<-ggraph(graphframe, layout="gem")+ # delete layout="gem" for default graph
  geom_edge_link(aes(color = node1.name,
                     start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)),
                 show.legend = FALSE, width = 0.5, arrow = arrow(length = unit(2, 'mm'))) +
  geom_node_text(aes(label = name, color = Popularitycut, size=Popularitycut), show.legend = FALSE) +
  scale_colour_manual(values=c("grey50","black")) + scale_size_manual(values=c(2,2.5)) +
  scale_edge_colour_manual(values=c("purple4","navyblue","burlywood3","aquamarine4",
                                    "cadetblue2","coral2","darkgoldenrod2","darkred"))+
  theme(plot.margin =  unit(c(0,0,0,0), "cm"))

# Patchwork of barplot and network together
theme_set(theme_minimal())
barplot + graph+
  plot_annotation(title="Top dessert ingredients in Chopped",
       subtitle="Blackberries are the most common ingredient. Ingredients are rarely repeated together!",
       caption="data from Kaggle | plot by @rjstevick for #TidyTuesday")+
  plot_layout(ncol = 2, widths = c(1, 3))

# Saving -----------------------------
ggsave("Chopped_plot.png", bg = "transparent", width = 12, height = 6, dpi = 400)
