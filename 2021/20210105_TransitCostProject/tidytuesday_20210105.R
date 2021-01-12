# Transit cost project
# TidyTuesday 2021 week 2
# Rebecca Stevick updated 1/12/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-01-05')

# Analysis and plotting ----------
tuesdata$transit_cost %>%
  drop_na(country) %>%
  # select the top 10 longest tunnel projects
  arrange(desc(tunnel)) %>% head(n = 10L) %>%
  # start plotting
  ggplot(aes(x=tunnel, y=reorder(city,tunnel), fill=country))+
  # add bar plot
  geom_col(alpha = 0.8)+
  # add construction year to the end of each bar
  geom_text(aes(x=tunnel-10, label = year), size = 7, color="white", family="Baloo")+
  # change the color of the bar
  scale_fill_manual(values = PNWColors::pnw_palette("Starfish", n=8))+
  # remove the extra white space in the x-axis
  scale_x_continuous(expand = c(0,0))+
  # change the global theme
  theme_minimal()+
  # edit the theme
  theme(text=element_text(family="Baloo"), # change font
        plot.title = element_text(hjust=0, size=24), # change position and size of title
        plot.caption = element_text(color="grey40"), # change color of the caption
        legend.position = "none", # remove the legend
        axis.text.y = element_text(size = 14))+ # make the y-axis text bigger
  # add those labels
  labs(title="Longest transit tunnels in the world",
       x="Tunnel Length (km)", y=NULL,
       caption = "data from the Transit Costs Project | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("TransitCostProject_plot.png", bg = "transparent", width = 9, height = 5, dpi = 400)
