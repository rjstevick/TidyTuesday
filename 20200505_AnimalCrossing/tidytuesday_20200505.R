# Animal Crossing
# TidyTuesday 2020 week 19
# RJS updated 5/5/2020

# Load libraries ---------------------

library(tidyverse)
library(ggalt)
library(ggtext)
library(scales)

sessionInfo()
theme_set(theme_light())

# Load data --------------------------

#critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
#user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
#items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

# Data formatting & analysis ---------

dumbbell_vil <-villagers %>% 
  group_by(gender) %>%
  count(species) %>%
  distinct() %>% 
  tidyr::spread(gender, n) %>%
  mutate(gap = `male` - `female`) %>%
  arrange(desc(gap)) %>%
  drop_na(gap)


# Plotting ---------------------------

ggplot(dumbbell_vil, aes(x = `female`, xend = `male`, 
                        y = reorder(species, gap), 
                        group = species)) + 
  geom_dumbbell(aes(color=gap), size = 4,
                colour_x = "darkmagenta",
                colour_xend = "#1380A1") +
  scale_color_gradient2(low="darkmagenta", mid="white", high="#1380A1")+
  labs(
    title = "**Are Villager species usually <span style='color:#1380A1;'>male</span> or
    <span style='color:darkmagenta;'>female</span>?**
    </span>",
    subtitle = "The most common <span style='color:#1380A1;'>male</span> Villager is a frog
    and the most common <span style='color:darkmagenta;'>female</span> is a cat
    </span>",
    caption = "Plot by @rjstevick \n Source: VillagerDB",
    x = "Number of Villagers", y = ""
    ) +
  theme(plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 0.5),
        text = element_text(size=18),
        legend.position = "none")

  
# Saving -----------------------------

ggsave("AnimalCrossing_plot.png", bg="transparent", width = 10.5, height = 6.5, dpi=400)
