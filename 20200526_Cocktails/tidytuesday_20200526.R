# Common cocktail recipes
# TidyTuesday 2020 week 22
# Rebecca Stevick updated 5/26/2020

# Load libraries ---------------------

library(tidyverse)
theme_set(theme_minimal())

# Load data --------------------------

#cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

boston_cocktails %>%
  # select 10 common cocktails
  filter(name=="Margarita" | name=="Mojito" | name=="Martini" | 
           name=="Daiquiri" | name=="Cosmopolitan" | name=="Manhattan Cocktail (dry)" | 
           name=="Sidecar" | name=="Moscow Mule" | name=="Gimlet" |
           name=="Long Island Iced Tea") %>%
  # separate out the ingredient measurements from the "oz"
  separate(measure, sep=" o", into=c("measurenum", "units"), remove=FALSE) %>%
  # make the measurements numeric
  mutate(measurenumeric=recode(measurenum, 
                               "1/2"=0.5, "1"=1, "1 1/2"=1.5, 
                               "2"=2, "3/4"=0.75, "3"=0.05, "7"=0.05)) %>%
  # add ginger beer to the moscow mule because it needs it
  add_row(name = "Moscow Mule", ingredient = "Ginger Beer", measurenumeric=4) %>%
  # add soda water to the mojito because it needs it
  add_row(name = "Mojito", ingredient = "Soda Water", measurenumeric=3) %>%
  # combine some ingredient names
  mutate(ingredient=recode(ingredient, 
                          "Simple Syrup, 1/2 oz"="Simple Syrup",
                          "Fresh Lime Juice"="Lime Juice",
                          "Old Thompson Blended Whiskey"="Whiskey",
                          "Mr. Boston Gin"="Gin",
                          "Fresh orange juice and orange wheel"="Fresh orange juice and slice",
                          "Cointreau or triple sec"="Triple Sec",
                          "Lime wedges"="Lime Juice")) %>%
  # simplify drink names
  mutate(name=recode(name,"Manhattan Cocktail (dry)"="Manhattan",
                     "Long Island Iced Tea"="Long Island \nIced Tea")) %>%
  # plotting time!
  ggplot(aes(x=name, y=measurenumeric,fill=ingredient))+
  # add bars
  geom_bar(stat="identity")+
  # define y-axis breaks and labels
  scale_y_continuous(NULL, breaks=c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
                     labels = scales::unit_format(accuracy=0.1, unit="oz"),
                     # remove the padding on the top and bottom
                     expand=c(0,0))+
  # move x-axis to top
  scale_x_discrete(NULL,position="top")+
  # define color scheme per ingredient
  scale_fill_manual(NULL,values=c("Lime Juice"="mediumspringgreen", 
                                  "Powdered Sugar"="peachpuff1",
                                  "Gin"="honeydew3","Triple Sec"="orange",                 
                                  "Dry Vermouth"="darkseagreen4","Cognac"="red3",
                                  "Fresh lemon juice"="yellow","Light Rum"="lightsalmon",
                                  "Vodka"="paleturquoise1", "Madeira"="darkred",                  
                                  "Fresh orange juice and slice"="darkorange3",
                                  "Blanco tequila"="lemonchiffon1",                                               
                                  "Simple Syrup"="lightsteelblue2",
                                  "Cranberry Juice"="indianred2",
                                  "Fresh mint leaves"="darkgreen",                  
                                  "Whiskey"="lightsalmon4",
                                  "Ginger Beer"="burlywood3",
                                  "Soda Water"="burlywood1"))+
  # move legend to bottom
  theme(legend.position="bottom",
        # remove axis lines
        panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        # thicken y-axis lines
        panel.grid.major.y = element_line(size=0.5,color="grey40"),
        # make text bigger and grey
        text = element_text(size=18, color="grey30"),
        axis.text.x = element_text(color="grey10", face="bold"),
        # center the title
        plot.title = element_text(hjust = 0.5),
        # make the caption smaller and more grey
        plot.caption = element_text(size=12, color="grey50"))+
  # add those labels
  labs(caption = "Plot by @rjstevick | Source: Kaggle",
       title = "Common Recipes for your geom_bartender()")


# Saving -----------------------------

ggsave("Cocktails_plot.png", bg="transparent", width = 12, height = 6.5, dpi=400)
