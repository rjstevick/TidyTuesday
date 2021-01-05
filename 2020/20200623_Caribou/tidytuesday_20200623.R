# Caribou Locations over time
# TidyTuesday 2020 week 26
# Rebecca Stevick updated 6/23/2020

# Load libraries
library(tidyverse)
library(ggmap)
library(PNWColors)
library(lubridate)
library(gganimate)
library(showtext)

theme_set(theme_minimal())

# Load data
individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

# Add a new column with year and season combined for animation
locations<- locations %>%
  # pull out year from timestamp
  mutate(year=year(timestamp)) %>%
  # order dataframe by year
  arrange(year) %>%
  # make new column with year and season
  unite(col="yearseason", season, year, sep=" ", remove=FALSE)

# Make the static plot with all observations shown
plotstat<-
  # load in the map data
  get_stamenmap(bbox = make_bbox(lat=c(52,58), lon=c(-132, -118)),
              crop = TRUE, zoom = 6) %>%
  # plot the map and set the coordinates
  ggmap() + coord_equal()+
  # add the caribou locations as hexes
  geom_hex(data=locations, aes(x=longitude, y=latitude),
           # make 5 bins for every lat/long
           binwidth = c(0.15, 0.15),
           # make the hexes a little transparent
           alpha=0.8,
           # add thin white borders to hexes
           color=alpha("white", 0.8), lwd=0.2)+
  # change the color scheme
  scale_fill_gradientn(colours=rev(pnw_palette("Moth", 10)),
                       # change to log scale so it's easier to see changes
                       trans="log",
                       # define where the labels/breaks are on the legend
                       breaks=c(1, 20, 400, 8000))+
  # fix where the legend is and its format
  theme(legend.position=c(0.12,0.15),legend.direction = "horizontal",
        # give the legend a transparent background
        legend.background = element_rect(fill=alpha("white", 0.5), color="transparent"),
        # add axis ticks for lat/long labels
        axis.ticks = element_line(inherit.blank=FALSE, color="grey30", size = 0.8),
        # make all the text grey
        text = element_text(size=12, color="grey30"),
        # change the size and color of the legend title
        legend.title = element_text(size=11),
        # make the main title bigger
        plot.title = element_text(size=16, family="HoltwoodOneSC"),
        # make the caption smaller and more grey
        plot.caption = element_text(size=12, color="grey50"),
        # transparent background
        plot.background = element_blank())+
  # put the legend title on the top
  guides(fill = guide_colourbar(title.position="top"))+
  # add those labels
  labs(x=NULL,y=NULL, fill="Number of caribou \nobservations (log scale)",
       caption = "Sources: Movebank, BC Ministry of Environment & ggmap \nPlot by @rjstevick for #TidyTuesday",
       title="Cumulative caribou locations 1988-2016")

#animate map based on year and season
plotanimate<-plotstat+
  transition_manual(frames=factor(yearseason, levels=unique(yearseason)), cumulative = FALSE)+
  # Leave faded hexes on the map
  #shadow_trail(alpha = 0.1)+
  # Add title with year/season shown
  ggtitle("Where did the caribou roam in {current_frame}?")


# Saving -----------------------------

# render animation
animate(plot = plotanimate,
        nframes = length(unique(locations$yearseason)),
        fps = 2, end_pause = 8,
        #define size and resolution of the gif
        height = 6, width = 12, units = "in", res = 100)

# save the animation
anim_save("Caribou_plot.gif")

# save the total static image too
ggsave(plot=plotstat, "Caribou_plot.png", bg="transparent", width = 12, height = 6.5, dpi=400)
