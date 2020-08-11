# Avatar: The Last Airbender
# TidyTuesday 2020 week 33
# Rebecca Stevick updated 8/11/2020

# Load libraries
library(tidyverse)
library(tvthemes) # for the Avatar theme
library(ggstream)

extrafont::loadfonts()

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-08-11')

tuesdata$avatar %>%
  filter(character != "Scene Description") %>% 
  group_by(book, book_num, chapter, chapter_num, character) %>% count() %>% 
  group_by(character) %>% mutate(sumn=sum(n)) %>% ungroup() %>% 
  mutate(characterOther=forcats::fct_lump_n(f=character, w=sumn, other_level="All other \ncharacters", n=7)) %>% 
  ggplot(aes(x=chapter_num, y=n, fill=reorder(characterOther, desc(sumn)))) +
  geom_stream(bw = 0.4, color = "white", alpha=0.7)+
  facet_grid(.~book)+
  scale_fill_avatar(palette = "FireNation")+
  scale_x_continuous(expand=c(0,0))+scale_y_continuous(labels = c(0,50,100,150,200))+
  theme_avatar(title.font = "Slayer", text.font = "Slayer")+
  theme(legend.position = "top", plot.caption = element_text(family="Montserrat", size=12), 
        axis.title = element_text(hjust=1), legend.background = element_rect(fill = NA, color = NA))+
  labs(fill=NULL, x="Chapter Number", y="Number of spoken lines",
       title="Avatar: the last airbender ",
       subtitle="Number of lines spoken by each character per episode per book",
       caption="data from Avatar wiki/Appa package | plot by @rjstevick for #TidyTuesday")

# Saving -----------------------------
ggsave("AvatarLastAirbender_plot.png", width = 10, height = 6, dpi = 400)

