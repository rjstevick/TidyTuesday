# African American History - US Census data
# TidyTuesday 2020 week 25
# Rebecca Stevick updated 6/16/2020

# Load libraries
library(tidyverse)
library(hrbrthemes)

# Load data - Important context on the datasets here: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-06-16/readme.md
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
# blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
# slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
# african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')

census %>%
  # put free and slave into one column
  gather(key="slave_free", value="number", black_free:black_slaves) %>%
  # remove the total values
  filter(division!="USA Total") %>%
  # group by region and year
  group_by(year, slave_free, region) %>%
  # sum up to get rid of the divisions
  summarise(sumnum=sum(number)) %>%
  # plotting time
  ggplot(aes(x=year, y=sumnum, fill=slave_free))+
  # put the year on the y-axis
  coord_flip()+
  # make a panel for each region
  facet_grid(.~region)+
  # add bar plot normalized to 1
  geom_col(position="fill", alpha=0.7)+
  # define colors and labels for the fill
  scale_fill_manual(labels=c("Free","Enslaved"),
                    values=c("grey50","black"))+
  # make the percent axis say %
  scale_y_continuous(labels=scales::percent_format())+
  # change the theme
  theme_ipsum()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color="gray75"),
        plot.background = element_blank(),
        axis.text = element_text(size=14, color="gray40", face="bold"),
        strip.text = element_text(color="gray40", size=18, face="bold"),
        legend.text = element_text(size=14, color="gray40", face="bold"),
        legend.position=c(0.92, 1.14),
        legend.direction = "horizontal")+
labs(x=NULL, y=NULL, fill=NULL,
       caption="Plot by @rjstevick | Source: US Census Archives",
       title="Census of US African-Americans and their status by region (1790 to 1870)")

# Saving -----------------------------
ggsave("AfricanAmericanHistory_plot.png", bg="transparent", width = 12, height = 6.5, dpi=400)
