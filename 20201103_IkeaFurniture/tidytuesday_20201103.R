# Ikea furniture!
# TidyTuesday 2020 week 45
# Rebecca Stevick updated 11/3/2020

# Load libraries -----------------
library(tidyverse)
library(ggalt)
library(ggtext)
library(hrbrthemes)
library(extrafont)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2020-11-03')
ikea <- tuesdata$ikea

# Analysis and plotting ----------
ikea %>%
  # only use products that have a previous price
  filter(old_price!="No old price") %>%
  # fix the old price column so it can be formatted as a number
  separate(old_price, into=c("SR","old_price", "multipack"), sep=" |/", remove=FALSE) %>%
  mutate(old_price=as.numeric(gsub(",", "",old_price))) %>%
  group_by(category) %>% # or group by category and name
  # convert saudi riyals to USD
  mutate(price_usd=price*0.27, old_price_usd=old_price*0.27) %>%
  # summary statistics
  summarise(meanprice=mean(price_usd),
            meanoldprice=mean(old_price_usd),
            # determine mean difference between previous and current price
            diff=meanprice-meanoldprice) %>%
  # start plotting
  ggplot(aes(x = meanprice, xend = meanoldprice, y = reorder(category, diff), group = category))+
  # add dumbbells based on previous and current prices
  geom_dumbbell(aes(color=diff), size = 3, shape=15, colour_x = "#ffcc00", colour_xend = "#003399")+
  # change color scheme
  scale_color_gradient2(low="#ffcc00", mid="white", high="#003399")+
  # add dollar signs to x-axis text
  scale_x_continuous(labels=scales::label_dollar(), limits=c(0,NA))+
  # change the overall theme
  theme_ipsum()+
  # edit the theme
  theme(legend.position = "none",
        plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 0.5))+
  # add those labels
  labs(title="IKEA items never increase in cost",
       subtitle="<span style='color:#ffcc00;'>**Current item prices**</span> are always lower than
                <span style='color:#003399;'>**past item prices**</span> in all categories.</span>",
       x="Mean price of category (USD $)",y=NULL,
       caption="data from Kaggle via IKEA | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("IkeaFurniture_plot.png", bg="transparent", width = 8, height = 5, dpi = 400)
