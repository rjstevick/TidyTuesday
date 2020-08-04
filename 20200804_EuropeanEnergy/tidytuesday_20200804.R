# European Energy treemap
# TidyTuesday 2020 week 32
# Rebecca Stevick updated 8/04/2020

# Load libraries
library(tidyverse)
library(hrbrthemes)
library(treemapify)
library(nationalparkcolors)

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-08-04')

# time to pipe, use energy_types
tuesdata$energy_types %>%
  # combine the year columns into one
  pivot_longer(names_to= "year", values_to="value", `2016`:`2018`) %>%
  # remove missing data and select only year 2018 and level 1 types
  drop_na(value, country_name) %>% filter(level=="Level 1" & year=="2018") %>% 
  # sum up the energy generated per country
  group_by(country) %>% mutate(sumValue=sum(value)) %>% ungroup() %>% 
  # make a new column with only the top 22 producing countries, label the others as "Others"
  mutate(countryOther=forcats::fct_lump_n(f=country_name, w=sumValue, other_level="Others", n=22)) %>% 
  # time to plot
  ggplot(aes(area = value, # area of each square is the energy generated per country/type
             fill = factor(type, levels=unique(type)), # keep the order of energy so that Other is last
             label = type, subgroup = countryOther))+
  # add the tree map and add borders between countries
  geom_treemap(color = "gray20") + geom_treemap_subgroup_border(color = "gray90", lwd=5)+
  # change the fill colors - based on rev(park_palette("CraterLake", n=7))
  scale_fill_manual(values = c("#4E7147", "#BE9C9D", "#376597","#7DCCD3", "#DBA662", "#9888A5", "#F7ECD8"))+ 
  # edit the text colors and sizes
  geom_treemap_text(aes(family=font_rc_light), colour = "gray75", place = "topleft", reflow = FALSE, size = 10)+
  geom_treemap_subgroup_text(aes(family=font_rc), col = 'white', size = 26) +
  # change theme and position the legend
  theme_ipsum_rc()+theme(legend.position = c(0.78,1.1), legend.direction = "horizontal")+
  # add labels
  labs(fill=NULL, title="European Energy Generation in 2018",
       subtitle="each area is proportional to energy generated per type & country in GWh (Gigawatt hours)", 
       caption="data from Eurostat Energy | plot by @rjstevick for #TidyTuesday")

# Saving -----------------------------
ggsave("EuropeanEnergy_plot.png", bg = "transparent", width = 12, height = 6, dpi = 400)



## try it with the treemap package as well
library(treemap)
tuesdata$energy_types %>%
  pivot_longer(names_to= "year", values_to="value", `2016`:`2018`) %>%
  drop_na(value, country_name) %>% filter(level=="Level 1" & year=="2018") %>% 
  group_by(country) %>% mutate(sumValue=sum(value)) %>% ungroup() %>% 
  mutate(countryOther=forcats::fct_lump_n(f=country_name, w=sumValue, other_level="Others", n=20)) %>% 
  treemap(index=c("countryOther", "type"),
          vSize = "value",  vColor="type", type="categorical",
          palette=c("#4E7147", "#BE9C9D", "#376597","#7DCCD3", "#DBA662", "#9888A5", "#F7ECD8"),
          overlap.labels = 0.4, fontsize.labels	= c(18, 12),
          title = "European Energy Generation in 2018",
          align.labels=list(c("center", "top"), c("right", "bottom")))
