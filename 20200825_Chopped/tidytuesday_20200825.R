# Chopped TV Show network
# TidyTuesday 2020 week 35
# Rebecca Stevick updated 8/27/2020

# Load libraries
library(tidyverse)
library(hrbrthemes)
library(widyr)
library(tidygraph)
library(ggraph)
library(patchwork)

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-08-25')
chopped <- tuesdata$chopped

# My first attempt at a network in awhile!
# Unfortunately, the ingredients aren't really connected and this got out of hand pretty quickly...

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

# Network of which ingredients are paired with the most common ingredients
comboingredientsgraph<-chopped %>%
  select(series_episode, dessert) %>%
  separate_rows(dessert, sep=", ") %>%
  # Make pairs of all ingredients for each episode
  pairwise_count(dessert, series_episode, sort=TRUE, upper=FALSE) %>% 
  # Get the top ingredient in the first column
  full_join(topingredients, by=c("item1"="dessert")) %>%
  full_join(topingredients, by=c("item2"="dessert")) %>%
  mutate(topingredient=coalesce(topingredient.x, topingredient.y)) %>% drop_na(topingredient) %>% 
  mutate(pairedingredient=case_when(is.na(topingredient.x) ~ item1, is.na(topingredient.y) ~ item2, TRUE ~ "other")) %>%
  # Build graph object
  select(topingredient, pairedingredient) %>%
  as_tbl_graph() %>% mutate(Popularity = centrality_power()) %>% mutate(Popularitycut=cut_interval(Popularity, n = 2))

# Plot the network
graph<-ggraph(comboingredientsgraph, layout="fr")+ 
  geom_edge_link(aes(color = node1.name, start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)),
                 show.legend = FALSE, width = 0.5, arrow = arrow(length = unit(1.5, 'mm'))) +
  geom_node_text(aes(label = name, color = Popularitycut, size=Popularitycut), show.legend = FALSE) +
  scale_colour_manual(values=c(alpha("grey50",0.6),"black")) + scale_size_manual(values=c(2,2.5)) +
  scale_edge_colour_manual(values=c("purple4","navyblue","burlywood3","aquamarine4",
                                    "cadetblue2","coral2","darkgoldenrod2","darkred"))+
  theme(plot.margin =  unit(c(0,0,0,0), "cm"), axis.text.x=element_blank(), axis.text.y=element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank())

# Patchwork of barplot and network together
theme_set(theme_ipsum(grid = ""))
barplot + graph + 
  plot_annotation(title="Top dessert ingredients in Chopped are never used together.",
                  subtitle="Blackberries are the most common ingredient. Ingredients are rarely repeated together!",
                  caption="data from Kaggle | plot by @rjstevick for #TidyTuesday")

# Saving -----------------------------
ggsave("Chopped_plot.png", bg = "transparent", width = 12, height = 6, dpi = 400)
