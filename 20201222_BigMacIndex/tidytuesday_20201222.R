# Big mac index
# TidyTuesday 2020 week 52
# Rebecca Stevick updated 12/22/2020

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-12-22')

# Analysis and plotting ----------
tuesdata$`big-mac` %>% 
  # remove countries without adjusted costs
  drop_na(usd_adjusted) %>% 
  # add variable with positive or negative
  mutate(costsign=as.character(sign(usd_adjusted))) %>% 
  # remove US since it's just 0
  filter(name!="United States") %>% 
  # start plotting
  ggplot(aes(x=date, y=usd_adjusted, fill=costsign)) +
  # add panel per country
  facet_wrap(.~name, ncol = 9) + 
  # add line at x-axis
  geom_hline(yintercept=0)+
  # add filled area
  geom_area() +
  # change fill color
  scale_fill_manual(values=c("firebrick4","goldenrod1")) +
  # change theme
  theme_ipsum() +
  # edit theme
  theme(legend.position = "none", panel.spacing = unit(0.15, "cm"),
        strip.text = element_text(family="Helvetica Bold", face="bold"),
        axis.text.x = element_text(size=7), axis.text.y = element_text(size=7),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        panel.background=element_rect(fill="#fffced", color="#fcf7dc")) +
  # add those labels
  labs(x=NULL, y="Big Mac Index, relative to $USD",
       title="Big Mac Purchasing Power",
       subtitle="The value of local currency: Cost of a local Big Mac, normalized to the cost of a Big Mac in the US and the local exhange rate",
       caption = "data from the The Economist | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("BigMacIndex_plot.png", bg = "transparent", width = 13.5, height = 7, dpi = 400)
