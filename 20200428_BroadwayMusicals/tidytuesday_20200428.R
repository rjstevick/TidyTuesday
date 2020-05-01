# Broadway musicals over time
# TidyTuesday 2020 week 18
# RJS updated 4/28/2020

# Load libraries ---------------------

library(tidyverse)
library(scales)
sessionInfo()

theme_set(theme_light())

# Load data --------------------------

grosses_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')


# Data formatting & analysis ---------

# Add in year column from the date stamp
grosses_raw$week_char<-as.character(grosses_raw$week_ending)
grosses<-grosses_raw %>%
  separate(week_char,into=c("year","month","date"), sep="-", remove = FALSE)

# Number of new plays per year
newplaydates<-grosses %>%
  group_by(show) %>%
  # find start date
  summarise(showstart=min(week_ending)) %>%
  # order it
  arrange(showstart)
# Add in year column from the date stamp
newplaydates$showstart_char<-as.character(newplaydates$showstart)
newplaysyears<-newplaydates %>% 
  separate(showstart_char,into=c("year","month","date"), sep="-") %>%
  select(show, year) %>% 
  group_by(year)

# Add in pre-1985 starts - it's a mess!
pre_1985_starts$start_date_char<-as.character(pre_1985_starts$start_date)
pre_1985_years<-pre_1985_starts %>% 
  separate(start_date_char,into=c("preyear","month","date"), sep="-") %>%
  select(show, preyear) %>% 
  group_by(preyear)
# combine variables
allnewyears<- newplaysyears %>% left_join(pre_1985_years)
allnewyears$year[1:19]<-allnewyears$preyear[1:19]
allnewyears<- select(allnewyears, c(show, year))
newplaysperyear<-allnewyears %>%
  group_by(year) %>%
  summarise(totalnew=n())
newplaysperyear$cumnew<-cumsum(newplaysperyear$totalnew)
pre1985totals<-newplaysperyear[1:6,]
pre1985totals<-rename(pre1985totals, sumshows=cumnew)

# Total shows per year
showsperyear<-grosses %>%
  group_by(year, show) %>%
  summarise(performances = sum(performances)) %>%
  group_by(year) %>%
  summarise(sumshows=n()) %>%
  # add in pre-1985 totals
  full_join(pre1985totals) %>%
  select(year, sumshows)

# Join data together, calculate old shown per year
allshowsnew<-
  left_join(newplaysperyear, showsperyear) %>%
  # subtract new shows from total shows
  mutate(totalold=sumshows-totalnew)

# Okay but there's some years missing!? fill them in...
yearframe<-data.frame(year=as.character(1975:2020))
# fill with the value above
allshowsnewcompl<-full_join(allshowsnew, yearframe) %>%
  arrange(year) %>%
  fill(totalold) %>%
  fill(sumshows) %>%
  fill(cumnew)
# fill in new shows with 0
allshowsnewcompl$totalnew<-replace_na(allshowsnewcompl$totalnew, 0)
# gather old and new into one column to plot
allshowsnewcomplgath<-gather(allshowsnewcompl,totalnew,totalold, key="oldnew", value="number") 


# Plotting -----------------------

plot1<-ggplot(showsperyear, aes(x=as.numeric(year), y=sumshows))+
  geom_point(alpha=0.8)+geom_line()+
  scale_fill_manual(labels=c("New plays","Old plays"), values=c("aquamarine3", "midnightblue"))+
  scale_x_continuous("Year",expand = c(0,0),  breaks = seq(1974, 2020, 4)) +
  scale_y_continuous("Number of shows playing",expand = c(0,0),  limits=c(0,90))+
  theme(panel.grid.minor = element_blank(), panel.border = element_rect(color="lightgrey"),
        title = element_text(color="grey25"), plot.background = element_blank(),
        legend.position = c(0.1, 0.85), legend.title = element_blank(),
        legend.background = element_rect(fill=NA), 
        legend.text = element_text(size=14))+
  labs(title="The number of Broadway shows playing every year is increasing")

plot2<-ggplot(allshowsnewcomplgath, aes(x=as.numeric(year), y=number, fill=oldnew))+
  geom_col(alpha=0.8, position="fill")+
  scale_fill_manual(labels=c("New shows","Old shows"), values=c("aquamarine3", "midnightblue"))+
  scale_x_continuous("Year",expand = c(0,0), breaks = seq(1974, 2020, 4)) +
  scale_y_continuous("Percent of shows playing",expand = c(0,0), labels = percent_format())+
  theme(panel.grid.minor = element_blank(), panel.border = element_rect(color="lightgrey"),
        title = element_text(color="grey25"), plot.background = element_blank(),
        legend.position = c(0.9, 0.2), legend.title = element_blank(),
        legend.text = element_text(size=12))+
  labs(title="But most Broadway shows are from previous years!")


# Saving --------------------------

cowplot::plot_grid(plot1, plot2,
                   align="hv", nrow=2, ncol=1)
ggsave("BroadwayShows_plot.png", bg="transparent", width = 8, height = 5.5, dpi=400)
