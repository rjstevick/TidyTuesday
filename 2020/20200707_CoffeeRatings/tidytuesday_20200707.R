# Coffee Ratings: top producers and locations
# TidyTuesday 2020 week 28
# Rebecca Stevick updated 7/8/2020

# Load libraries
library(tidyverse)

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-07-07')
coffee_ratings <- tuesdata$coffee_ratings

# make a barplot of number of coffees per country
coffeebar<-coffee_ratings %>% 
  # group by country of origin
  group_by(country_of_origin) %>%
  # count number of beans rated per country
  count() %>% drop_na() %>%
  # fix some country names
  mutate(country_of_origin=recode(country_of_origin,
                                  "Cote d?Ivoire"="Ivory Coast",
                                  "Tanzania, United Republic Of"="Tanzania",
                                  "United States"="USA")) %>%
  # plotting time
  ggplot(aes(x=reorder(country_of_origin, -n), y=n, fill=n)) + geom_col() +
  # change color of fill
  scale_fill_gradient(low="#ddc7b8", high="#382b22") +
  # remove extra white space
  scale_y_continuous(expand=c(0,0)) +
  # change themes
  coord_flip() + theme_minimal() + 
  theme(legend.position="none",panel.grid = element_blank(),
        text = element_text(color="#4b392d", family="Arial Narrow", size=8),
        axis.ticks.x = element_line(inherit.blank = FALSE)) +
  labs(x=NULL, y=NULL, fill=NULL)

# make a map with barplot in the corner
coffee_ratings %>% 
  # group by country of origin
  group_by(country_of_origin) %>%
  # count number of beans rated per country
  count() %>% drop_na() %>%
  # fix some country names
  mutate(country_of_origin=recode(country_of_origin,
                                  "Cote d?Ivoire"="Ivory Coast",
                                  "Tanzania, United Republic Of"="Tanzania",
                                  "United States"="USA")) %>%
  # add in map data based on the country
  left_join(map_data("world"), by=c("country_of_origin"="region")) %>%
  # start plotting
  ggplot(aes(x=long, y=lat, group=group)) + 
  # add world map
  geom_polygon(data=map_data("world"), fill="grey90", color="white") +
  # add coffee locations, colored by number of beans rated
  geom_polygon(aes(fill=n), color="#f1e8e2") +
  # change color of fill
  scale_fill_gradient("Number of coffee \nbean varieties graded", low="#ddc7b8", high="#382b22") +
  # remove all theme elements and fix the x-y so the map doesn't warp
  theme_void() + coord_fixed(1.3) + 
  # remove extra white space
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
  # extra theme changed
  theme(legend.position=c(0.75,0.14), legend.direction = "horizontal",
        text = element_text(color="#4b392d", family="Arial Narrow", face="bold"),
        plot.title = element_text(hjust = 0.5, size=18))+
  # put the legend title on the top and centered
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))+
  # add those labels
  labs(title="Where are coffee beans grown around the world?",
       caption = "Source: James LeDoux & Coffee Quality Database \nPlot by @rjstevick")+ 
  # add the bar plot on the bottom left of the map
  annotation_custom(ggplotGrob(coffeebar),
                               xmin = -175, ymin = -78,
                               xmax = -15, ymax=15)


# Saving -----------------------------
ggsave("CoffeeRatings_plot.png", bg="transparent", width = 8, height = 5.6, dpi=400)
