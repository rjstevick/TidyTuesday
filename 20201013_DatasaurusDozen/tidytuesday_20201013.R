# Datasaurus Dozen
# TidyTuesday 2020 week 42
# Rebecca Stevick updated 10/13/2020

# Load libraries ---------------
library(tidyverse)
library(patchwork)
library(nationalparkcolors)

# Load data --------------------
tuesdata <- tidytuesdayR::tt_load('2020-10-13')
datasaurus <- tuesdata$datasaurus

# Analysis and plotting --------

datasaurus %>% group_by(dataset) %>% summarise_all(list(mean=mean, median=median))

plots<-datasaurus %>% ggplot(aes(x=x, y=y))+
  facet_wrap(~dataset, ncol=3) + 
  geom_density_2d_filled()+ 
  geom_point(aes(color=dataset))+ labs(x=NULL, y=NULL)+
  scale_color_manual(values=c(park_palette("GeneralGrant"),park_palette("CraterLake")))+
  theme_minimal() + theme(legend.position = "none")

ybox<-datasaurus %>% ggplot(aes(x=dataset, y=y, fill=dataset))+coord_flip()+
  geom_boxplot()+labs(x=NULL)+theme_minimal()+ theme(legend.position = "none")+
  scale_fill_manual(values=c(park_palette("GeneralGrant"),park_palette("CraterLake")))
xbox<-datasaurus %>% ggplot(aes(x=dataset, y=x, fill=dataset))+
  geom_boxplot()+labs(x=NULL)+theme_minimal()+ theme(legend.position = "none", axis.text.x = element_blank())+
  scale_fill_manual(values=c(park_palette("GeneralGrant"),park_palette("CraterLake")))

ybox+plots+plot_spacer()+xbox+plot_layout(widths=c(1,4), heights=c(3,1))+
  plot_annotation(title="Not all medians are created equal... ",
                  subtitle="Datasets from datasauRus all have the same mean and median, but different x-y shapes.",
                  caption="data from datasauRus R package | plot by @rjstevick for #TidyTuesday", 
                  theme=theme(text = element_text('mono'), plot.title = element_text(size=18, face="bold")))

# Saving -----------------------
ggsave("DatasaurusDozen_plot.png", bg="transparent", width = 11, height = 8, dpi = 400)
