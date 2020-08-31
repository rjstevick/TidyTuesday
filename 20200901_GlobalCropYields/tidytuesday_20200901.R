# Global crop yields
# TidyTuesday 2020 week 36
# Rebecca Stevick updated 9/1/2020

# Load libraries ---------------
library(tidyverse)
library(geofacet)

# Load data --------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-01')
key_crop_yields <- tuesdata$key_crop_yields

# Analysis and plotting --------
# Clean up potato-growing countries
potatocountries<- key_crop_yields %>% 
  drop_na(`Potatoes (tonnes per hectare)`) %>% drop_na(Code) %>% 
  filter(!grepl("OWID", Code)) 

# Edit world grid and add extra countries
world_countries_grid1_edit<-world_countries_grid1 %>% 
  # since geo_facet doesn't accept blanks, only use these columns
  select(name, code_alpha3, col, row) %>% 
  add_row(name="Bermuda", code_alpha3="BMU", col=3, row=2)%>%
  add_row(name="Faeroe Islands", code_alpha3="FRO", col=13, row=2)%>%
  add_row(name="French Polynesia", code_alpha3="PYF", col=28, row=20)%>%
  add_row(name="Guadeloupe", code_alpha3="GLP", col=8, row=5)%>%
  add_row(name="Montserrat", code_alpha3="MSR", col=6, row=6)%>%
  add_row(name="New Caledonia", code_alpha3="NCL", col=25, row=20)%>%
  add_row(name="Palestine", code_alpha3="PSE", col=19, row=10)%>% 
      filter(code_alpha3!="QAT") %>%  filter(code_alpha3!="ARE") %>% 
      add_row(name="Qatar", code_alpha3="QAT", col=20, row=10)%>% 
      add_row(name="United Arab Emirates", code_alpha3="ARE", col=21, row=10)%>% 
  add_row(name="Reunion", code_alpha3="REU", col=20, row=19)%>% 
  add_row(name="Taiwan", code_alpha3="TWN", col=25, row=8) %>% 
  # remove countries that don't grow potatoes
  filter(code_alpha3 %in% potatocountries$Code) %>% 
  # make new name column that's wrapped for plotting
  mutate(name=recode(name, "Congo (Democratic Republic of the)"="DR of the Congo")) %>% 
  mutate(namewrap=str_wrap(name, width=15))

# Plot potatoes grown over time on the world grid
potatocountries %>% 
  ggplot(aes(x=Year, y=0,
             color=`Potatoes (tonnes per hectare)`, 
             size=`Potatoes (tonnes per hectare)`)) +
  geom_point(alpha=0.8)+theme(legend.position = "none")+
  scale_color_viridis_c()+
  facet_geo(~Code, grid=world_countries_grid1_edit, label = "namewrap")+
  theme_void()+
  theme(strip.text = element_text(size=7))+
  labs(title="Potatoes Around the World",
       subtitle="",
       caption="data from Our World in Data | plot by @rjstevick for #TidyTuesday")

# Saving -----------------------
ggsave("GlobalCropYields_plot.png", bg = "transparent", width = 12, height = 6.5, dpi = 400)
