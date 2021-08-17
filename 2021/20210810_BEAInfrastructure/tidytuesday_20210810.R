# BEA Infrastructure Investment
# TidyTuesday 2021 week 33
# Rebecca Stevick updated 8/17/2021

# Load libraries -----------------
library(tidyverse)
library(ggstream)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-08-10')
investment <- tuesdata$investment

# Analysis and plotting ----------
investment %>%
   filter(meta_cat=="Transportation" &
             category %in% c("Highways and streets", "Air transportation", "Water transportation",
                          "Rail transportation", "Transit") &
             gross_inv > 0) -> cleaninvest

# plotting
ggplot()+
   geom_area(data=cleaninvest, aes(x=year, y=gross_inv, fill=reorder(category, -gross_inv)),
             alpha = 0.7, color = "white", position="fill")+
   geom_text(aes(x=c(2015, 1986, 2002, 1955, 2003.5), y=c(0.8, 0.25, .15, .1, .02),
                 label=c("Highways and streets", "Transit", "Air", "Rail", "Water")),
                 color=c("black","black","black","black","grey80"),
             hjust=1, family="Copperplate", size=5)+
   scale_x_continuous(expand=c(0,0))+
   scale_y_continuous(expand=c(0,0), labels = scales::label_percent())+
   scale_fill_manual(values=wesanderson::wes_palette("Darjeeling2", 5))+
   theme_void()+
   theme(text = element_text(family = "Copperplate"),
         axis.text.y = element_text(inherit.blank = FALSE, size=12, hjust=1),
         axis.text.x = element_text(inherit.blank = FALSE, size=16),
         panel.grid.major.y = element_line(inherit.blank = FALSE),
         plot.title = element_text(hjust=0.5, size=24),
         plot.subtitle = element_text(hjust=0.5),
         legend.position = "none")+
   # add those labels
   labs(title = "Transportation Investments in the US since 1960",
        subtitle = "Rail transportation investments decreased in 1960, and were replaced by transit and air transportation. \n",
        fill = NULL,
        caption = "data from Bureau of Economic Analysis | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("BEAInfrastructure_plot.png", bg = "transparent", width = 9, height = 4, dpi = 400)
