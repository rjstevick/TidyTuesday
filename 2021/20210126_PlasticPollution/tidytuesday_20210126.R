# Plastic Pollution
# TidyTuesday 2021 week 5
# Rebecca Stevick updated 1/26/2021

# Load libraries -----------------
library(tidyverse)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-01-26')

# Analysis and plotting ----------
tuesdata$plastics %>% 
  filter(country=="United States of America" & year==2019) %>%  
  group_by(parent_company) %>% summarise_at(vars(hdpe:grand_total), sum, na.rm=TRUE) %>% 
  pivot_longer(hdpe:pvc, names_to="plastic_type", values_to="count") %>% 
  filter(parent_company!="Grand Total" & parent_company!="Unbranded") %>% 
  filter(grand_total>10) %>% 
  # remove plastic types that don't exist in this subset of data
  filter(count!=0) %>% 
  # start plotting
  ggplot(aes(x=count, y=reorder(parent_company,-grand_total), fill=plastic_type))+
  geom_col()+
  scale_fill_manual(values = nationalparkcolors::park_palette("Denali"),
                    limits = c("hdpe", "ldpe", "pet", "pp", "o"),
                    labels = c("Polyethylene, high density", "Polyethylene, low density", "Polyester", "Polypropylene", "Other"))+
  scale_x_continuous(expand = c(0,0))+
  theme_minimal()+
  theme(text = element_text(size = 16, family = "Cochin"), legend.position = c(0.75,0.7),
        plot.title = element_text(size = 28, family = "Copperplate"), plot.subtitle = element_text(size=12))+
  # add those labels
  labs(title = "All the polys", 
       subtitle = "Most abundant plastic pollution types and sources in the US in 2019",
       x = NULL, y = NULL, fill = "Plastic Type",
       caption = "data from Break Free from Plastic | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("PlasticPollution_plot.png", bg = "transparent", width = 9, height = 5, dpi = 400)
