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
  filter(old_price!="No old price") %>% 
  separate(old_price, into=c("SR","old_price", "multipack"), sep=" |/", remove=FALSE) %>% 
  group_by(category) %>% # or group by category and name
  # convert saudi riyals to USD
  mutate(price_usd=price*0.27, 
         old_price=gsub(",", "",old_price),
         old_price_usd=as.numeric(old_price)*0.27) %>% 
  # summary statistics
  summarise(meanprice=mean(price_usd),
            meanoldprice=mean(old_price_usd),
            diff=meanprice-meanoldprice) %>% 
  # start plotting
  ggplot(aes(x = meanprice, xend = meanoldprice, 
             y = reorder(category, diff), group = category))+
  geom_dumbbell(aes(color=diff), size = 4,
                colour_x = "#ffcc00", colour_xend = "#003399")+
  scale_color_gradient2(low="#ffcc00", mid="white", high="#003399")+
  scale_x_continuous(labels=scales::label_dollar(), limits=c(0,NA))+
  theme_ipsum()+
  theme(legend.position = "none",
        plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 0.5))+
  # add those labels
  labs(title="IKEA items never increase in cost",
       subtitle="<span style='color:#ffcc00;'>**Current item prices**</span> are always lower than
                <span style='color:#003399;'>**old item prices**</span> in all categories.</span>",
       x="Mean price of category (USD $)",y=NULL,
       caption="data from Kaggle via IKEA | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("IkeaFurniture_plot.png", bg="transparent", width = 8, height = 5, dpi = 400)
