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

# this got out of hand retty quickly...
# not the best code, but it gets the job done.

# Plot most common dessert ingredients
topingredients <- chopped %>%
  select(dessert, series_episode) %>% 
  separate_rows(dessert, sep=", ") %>% 
  group_by(dessert) %>% count() %>% ungroup() %>% 
  filter(n>=8) %>%
  mutate(topingredient=dessert)
barplot<-ggplot(topingredients, aes(x = n, y = reorder(dessert,-n))) +
  geom_segment(aes(x = 0, y = reorder(dessert,-n), 
                   xend = n, yend = reorder(dessert,-n)), 
               color = "chocolate4",alpha = 0.6, lwd = 1.5)+
  geom_point(aes(color=dessert), size = 4, alpha = 0.9)+
  scale_x_continuous(expand = c(0,0.2), breaks = scales::breaks_pretty(n=5))+
  scale_color_manual(name=NULL, values=c(park_palette("GeneralGrant"),rev(park_palette("Yellowstone"))))+
  theme_ipsum()+
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_text(size=12), legend.position="none")+
  labs(x="Number of episodes", y=NULL)

# Plot which ingredients are paired with the most common ingredients
full_table<-chopped %>%
  select(series_episode, dessert) %>% 
  separate(dessert, sep=", ", into=c("a1", "a2", "a3", "a4"))

comboingredients<-
  full_table[1:3] %>% 
  full_join(full_table[c(1,2,4)], by=c("a1"="a1","a2"="a3", "series_episode"="series_episode")) %>% 
  full_join(full_table[c(1,3,4)], by=c("a1"="a2","a2"="a3", "series_episode"="series_episode")) %>% 
  full_join(full_table[c(1,2,5)], by=c("a1"="a1","a2"="a4", "series_episode"="series_episode")) %>% 
  full_join(full_table[c(1,3,5)], by=c("a1"="a2","a2"="a4", "series_episode"="series_episode")) %>% 
  full_join(full_table[c(1,4,5)], by=c("a1"="a3","a2"="a4", "series_episode"="series_episode")) %>% 
  drop_na(a1, a2) %>% 
  group_by(G1 = pmin(a1, a2), G2 = pmax(a1, a2)) %>% 
  add_tally(name="comboN") %>% mutate(comboN=as.factor(comboN)) %>% 
  arrange(desc(comboN)) %>% 
  group_by(G1) %>% add_count(name="ingredientN1") %>%
  group_by(G2) %>% add_count(name="ingredientN2")

graphframe<-comboingredients %>% 
  filter(G1 %in% topingredients$dessert | G2 %in% topingredients$dessert) %>%
  full_join(topingredients, by=c("G1"="dessert")) %>% 
  full_join(topingredients, by=c("G2"="dessert")) %>% 
  mutate(n=coalesce(n.x, n.y)) %>% 
  mutate(topingredient=coalesce(topingredient.x, topingredient.y)) %>% 
  mutate(pairedingredient=case_when(is.na(topingredient.x) ~ G1,
                                    is.na(topingredient.y) ~ G2,
                                    TRUE ~ "other")) %>% 
  ungroup() %>% 
  select(topingredient, pairedingredient, series_episode) %>% 
  group_by_all() %>% 
  summarize(n = n()) %>% 
  drop_na() %>% 
  as_tbl_graph()

graph<-ggraph(graphframe, layout="gem")+ # delete layout="gem" for default graph 
  geom_edge_link(aes(alpha = n,
                     start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name),
                     color = node1.name),
                 show.legend = FALSE,
                 arrow = arrow(length = unit(2, 'mm')),
                 width = 0.5) + 
  geom_node_text(aes(label = name),
                 show.legend = FALSE, size = 2.5) +
  scale_edge_colour_manual(values=c(park_palette("GeneralGrant"),rev(park_palette("Yellowstone"))))

theme_set(theme_minimal())
graph+barplot+
  plot_annotation(title="Top dessert ingredients in Chopped",
       subtitle="Blackberries are the most common ingredient. Ingredients are rarely repeated together!",
       caption="data from Kaggle | plot by @rjstevick for #TidyTuesday")+ 
  plot_layout(ncol = 2, widths = c(2, 1))



# Saving -----------------------------
ggsave("Chopped_plot.png", bg="transparent", width = 12, height = 7, dpi = 400)
