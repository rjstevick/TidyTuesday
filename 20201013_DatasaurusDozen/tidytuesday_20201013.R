# Datasaurus Dozen
# TidyTuesday 2020 week 42
# Rebecca Stevick updated 10/13/2020

# Load libraries ---------------
library(tidyverse)
library(ggExtra)
library(patchwork)

# Load data --------------------
tuesdata <- tidytuesdayR::tt_load('2020-10-13')
datasaurus <- tuesdata$datasaurus

# Analysis and plotting --------

datasaurus %>% group_by(dataset) %>% summarise_all(mean)

plots<-datasaurus %>% ggplot(aes(x=x, y=y))+
  facet_wrap(~dataset, ncol=3) + 
  geom_density_2d_filled(alpha = 0.6)+ 
  geom_point(alpha=0.8)+ labs(x=NULL, y=NULL)+
  theme_minimal() + theme(legend.position = "none")

ybox<-datasaurus %>% ggplot(aes(x=dataset, y=y))+
  geom_boxplot()+labs(x=NULL)+theme_minimal()+coord_flip()
xbox<-datasaurus %>% ggplot(aes(x=dataset, y=x))+
  geom_boxplot()+labs(x=NULL)+theme_minimal()

(ybox+plots)/(plot_spacer()+xbox)+
  plot_annotation(title="",
                  caption="data from datasauRus R package | plot by @rjstevick for #TidyTuesday")


# Saving -----------------------
ggsave("DatasaurusDozen_plot.png", bg="transparent", width = 10, height = 5, dpi = 400)
