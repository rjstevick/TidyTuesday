# US droughts
# TidyTuesday 2021 week 30
# Rebecca Stevick updated 8/5/2021

# Load libraries -----------------
library(tidyverse)
library(extrafont)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-07-20')
drought <- tuesdata$drought

# Analysis and plotting ----------
drought %>%
  filter(state_abb=="CA") %>% 
  ggplot(aes(y = area_pct, x=valid_start, fill=drought_lvl))+
  geom_col(position="fill")+
  scale_fill_manual(values=c(PNWColors::pnw_palette("Sunset", 5), "grey80"))+
  scale_y_continuous(labels = scales::label_percent(), expand=c(0,0))+
  scale_x_date(labels = scales::label_date(format="%m/%Y"), breaks = scales::date_breaks("2 years"), expand = c(0,0))+
  theme_minimal()+
  theme(text = element_text(family="Montserrat"), legend.position = c(0.8, 0.14),
        legend.background = element_rect(fill = alpha("white", 0.5), color = "transparent"),
        plot.title = element_text(face = "bold", size = 24, family = "Montserrat Black",
                                  vjust = -10, hjust = 0.1, color = "white"),
        axis.title.y = element_text(hjust=1))+
  guides(fill = guide_legend(nrow = 1)) +
  # add those labels
   labs(title = "Calfornia drought status since 2001",
        x = NULL, y = "Percent of state per category", fill = "Drought Category",
        caption = "data from U.S. Drought Monitor | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("USDroughts_plot.png", bg = "transparent", width = 11, height = 5, dpi = 400)
