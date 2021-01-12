# Transit cost project
# TidyTuesday 2021 week 2
# Rebecca Stevick updated 1/5/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-01-05')

# Analysis and plotting ----------
tuesdata$transit_cost %>%
  drop_na(country) %>%
  arrange(desc(tunnel)) %>% head(n = 10L) %>%
  ggplot(aes(x=tunnel, y=reorder(city,tunnel), fill=country))+
  geom_col(alpha = 0.8)+
  geom_text(aes(x=tunnel-10, label = year), size = 7, color="white", family="Baloo")+
  scale_fill_manual(values = PNWColors::pnw_palette("Starfish", n=8))+
  scale_x_continuous(expand = c(0,0))+
  theme_minimal()+
  theme(text=element_text(family="Baloo"),
        plot.title = element_text(hjust=0, size=24), plot.caption = element_text(color="grey40"),
        legend.position = "none", axis.text.y = element_text(size = 14))+
  # add those labels
  labs(title="Longest transit tunnels in the world",
       x="Tunnel Length (km)", y=NULL,
       caption = "data from the Transit Costs Project | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("TransitCostProject_plot.png", bg = "transparent", width = 9, height = 5, dpi = 400)
