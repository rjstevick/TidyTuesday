# Global crop yields
# TidyTuesday 2020 week 36
# Rebecca Stevick updated 9/1/2020

# Load libraries ---------------
library(tidyverse)
library(geofacet)
library(ggimage)

# Load data --------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-01')
key_crop_yields <- tuesdata$key_crop_yields

# Analysis and formatting ------

# Import potato image
potato<-"https://static.thenounproject.com/png/26079-200.png"

# Clean up potato-growing countries
potatocountries<- key_crop_yields %>%
  drop_na(`Potatoes (tonnes per hectare)`) %>% drop_na(Code) %>%
  filter(!grepl("OWID", Code)) %>%
  # group yearly info by decade
  mutate(Decade=as.factor(cut_width(Year, width=10,
                                    labels=c("1955-1965","1966-1975","1976-1985",
                                      "1986-1995","1996-2005","2006-2015","(2015,2025]")))) %>%
  group_by(Entity, Code, Decade) %>% summarise(Potatoes=sum(`Potatoes (tonnes per hectare)`)) %>%
  filter(Decade!="(2015,2025]") # remove partial decade

# Edit world grid and add extra countries
world_countries_grid1_edit<-world_countries_grid1 %>%
  # since geo_facet doesn't accept blanks, only use these columns
  select(name, code_alpha3, col, row) %>%
  # add in the countries with potatoes that aren't in "world_countries_grid1"
  add_row(name="Bermuda", code_alpha3="BMU", col=3, row=2)%>%
  add_row(name="Faeroe Islands", code_alpha3="FRO", col=13, row=2)%>%
  add_row(name="French Polynesia", code_alpha3="PYF", col=27, row=20)%>%
  add_row(name="Guadeloupe", code_alpha3="GLP", col=8, row=5)%>%
  add_row(name="Montserrat", code_alpha3="MSR", col=6, row=6)%>%
  add_row(name="New Caledonia", code_alpha3="NCL", col=25, row=20)%>%
  add_row(name="Reunion", code_alpha3="REU", col=20, row=19)%>%
  add_row(name="Taiwan", code_alpha3="TWN", col=25, row=8) %>%
  add_row(name="Palestine", code_alpha3="PSE", col=19, row=10)%>%
      # Since we put Palestine where Qatar is, move QAT and ARE over to the right
      filter(code_alpha3!="QAT") %>%  filter(code_alpha3!="ARE") %>%
      add_row(name="Qatar", code_alpha3="QAT", col=20, row=10)%>%
      add_row(name="United Arab Emirates", code_alpha3="ARE", col=21, row=10)%>%
  # remove countries that don't grow potatoes
  filter(code_alpha3 %in% potatocountries$Code)

# Plotting ---------------------

# Plot sum total potatoes grown over time in the world
globalpotatoplot<-potatocountries %>%
  group_by(Decade) %>%
  summarise(globalpotatoes=sum(Potatoes)) %>%
  ggplot(aes(x=Decade, y=0)) +
  geom_point(aes(color=globalpotatoes, size=globalpotatoes/2000))+
  geom_image(aes(image=potato, size=globalpotatoes/150000),
             by="height")+scale_size_identity()+
  geom_text(aes(label=Decade), nudge_y=-0.003, size=2.2)+
  scale_color_gradient2(name="Global potatoes (tons per hectare per decade)", low="bisque3", high="#6e6355")+
  scale_y_continuous(expand=c(0,0), limits=c(-0.01, 0.01))+
  theme_void()+theme(legend.position = c(0.5, 0.7), legend.direction = "horizontal",
                     legend.key.width = unit(1.7, "cm"), legend.key.height = unit(0.3, "cm"),
                     text=element_text(size=8))+
                     guides(color = guide_colourbar(title.position="top", title.hjust = 0.5))

# Plot potatoes grown over time on the world grid
geofacetplot<-potatocountries %>%
  ggplot(aes(x=Decade, y=0, fill=Potatoes)) + geom_tile()+
  # add country name on top of mini heatmaps
  geom_text(aes(x="1986-1995", label=Code), size=4, color="darkolivegreen", family="Arial Narrow")+
  # change fill color
  scale_fill_gradient2(name="Potatoes (tons per hectare per decade)", low="bisque3", high="#6e6355")+
  # remove extra space on y-axis
  scale_y_continuous(expand=c(0,0))+
  # create a panel per country in its location on a world map
  facet_geo(~Code, grid=world_countries_grid1_edit)+
  # add blank theme
  theme_void()+
  # edit plot theme
  theme(plot.title = element_text(size=32, family="Arial Narrow", face="bold", color="#423b33"),
        plot.subtitle = element_text(size=18, family="Arial Narrow", color="#423b33"),
        plot.caption = element_text(size=12, family="Arial Narrow", color="#423b33"),
        strip.text = element_blank(), legend.position = c(0.8,0.9), legend.direction = "horizontal",
        legend.key.width = unit(1, "cm"))+
        guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))+
  # add those labels
  labs(title="Global potato production is increasing around the world",
       subtitle="Each mini heatmap indicates the total potato production in each country per decade (1955-2015)",
       caption="data from Our World in Data | plot by @rjstevick for #TidyTuesday")

# Could not add the global potato plot directly to the geo facets, since we don't have x,y coordinates
# So, create a blank plot
qplot(1:10, 1:10, color=I("transparent"))+theme_void() +
  # then add the geofacet plots
  annotation_custom(grob = ggplotGrob(geofacetplot),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  # then inset the global potato plot on the bottom left
  annotation_custom(grob = ggplotGrob(globalpotatoplot),
    xmin = 1, xmax = 3.5, ymin = 0, ymax = 3.5)

# Saving -----------------------
ggsave("GlobalCropYields_plot.png", bg = "transparent", width = 12, height = 6.5, dpi = 400)
