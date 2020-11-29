# Washington Trails
# TidyTuesday 2020 week 48
# Rebecca Stevick updated 11/24/2020

# Load libraries -----------------
library(tidyverse)
library(extrafont)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-11-24')

# Analysis and plotting ----------
tuesdata$hike_data %>%
  # separate out key words in features and then wrap the words
  unnest(features) %>% mutate(labelfeatures=str_wrap(features, width=12)) %>%
  # calculate mean rating per feature keyword
  group_by(labelfeatures) %>% summarise(meanrate=mean(as.numeric(rating), na.rm=TRUE)) %>%
  # start plotting
  ggplot(aes(x=reorder(labelfeatures,meanrate), y=meanrate, fill=meanrate))+
  # add columns and switch to polar plot
  geom_col(alpha = 0.8) + coord_polar(clip = "off")+
  # edit the fill color
  scale_fill_gradient(high='darkgreen', low='grey65', breaks=c(3,3.25,3.5))+
  # change the global theme
  theme_minimal()+
  # edit the theme
  theme(text = element_text(family="Montserrat"), legend.position = "bottom", legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(face="bold", hjust=0.5, size=18), plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5), axis.text.y = element_blank(), axis.text.x = element_text(size=12))+
  # center the legend title
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))+
  # add those labels
  labs(title="The Trail Ratings Paradox",
       subtitle="Washington trails that do not allow dogs have the highest ratings... \nand trails with coast views have the lowest ratings.",
       x=NULL, y=NULL, fill="Mean Rating per keyword",
       caption = "data from Washington Trails Association | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("WashingtonTrails_plot.png", bg = "transparent", width = 7, height = 7, dpi = 400)
