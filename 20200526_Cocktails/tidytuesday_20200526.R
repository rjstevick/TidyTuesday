# Cocktails
# TidyTuesday 2020 week 22
# Rebecca Stevick updated 5/26/2020

# Load libraries ---------------------

library(tidyverse)
library(ggimage)

#library(ggalt)
#library(ggtext)
#library(scales)

sessionInfo()
theme_set(theme_light())

# Load data --------------------------

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

# Data formatting & analysis ---------

# ingredients per drink
commoningredients <-cocktails %>% 
  group_by(ingredient, drink) %>%
  dplyr::count(sort=TRUE) %>% 
  spread(ingredient, n)


new<-cocktails %>%
  filter(category=="Cocktail") %>%
  group_by(drink) %>%
  reshape2::dcast(ingredient ~drink, 
                  drop=TRUE, length) %>%
  group_by(ingredient)%>%
  summarise_all(sum)

%>%
  as.matrix()



res <- reshape2::dcast(
  reshape2::melt(cocktails, id.vars = c("drink","ingredient")),
  variable+ingredient ~ drink,
  value.var = "value")

library(chorddiag)
#groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223")
chorddiag(new, groupColors = groupColors, groupnamePadding = 20)


# Plotting ---------------------------

library(ggalluvial)
ggplot(as.data.frame(commoningredients),
       aes(x=ingredient_number,
           stratum=ingredient,
           alluvium=ingredient,
           y = n, 
           fill = ingredient, 
           label=ingredient)) +
  geom_flow(width = 1/12) +
  geom_stratum(width = 1/12, alpha=0.8) +
 # geom_label(stat = "stratum", infer.label = TRUE) +
  scale_x_discrete(expand = c(.05, .05))+
  theme(legend.position = "none")
 # scale_fill_brewer(type = "qual", palette = "Set1")



# Saving -----------------------------

ggsave("Cocktails_plot.png", bg="transparent", width = 10.5, height = 6.5, dpi=400)
