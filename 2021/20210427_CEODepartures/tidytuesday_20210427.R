# CEO Departures
# TidyTuesday 2021 week 18
# Rebecca Stevick updated 4/27/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-04-27')
departures <- tuesdata$departures

# Analysis and plotting ----------
departures %>%
   filter(fyear <= 2016 & fyear >= 1990) %>%
   mutate(reason = case_when(departure_code == 1 ~ "Involuntary",
                             departure_code == 2 ~ "Involuntary",
                             departure_code == 3 ~ "Involuntary",
                             departure_code == 4 ~ "Involuntary",
                             departure_code == 5 ~ "Voluntary",
                             departure_code == 6 ~ "Voluntary",
                             departure_code == 7 ~ "Other Reason",
                             departure_code == 8 ~ "Missing",
                             departure_code == 9 ~ "Other Reason",
                             TRUE ~ "Missing"),
          reason = factor(reason, levels = c("Voluntary", "Involuntary", "Other Reason", "Missing"))) %>%
   group_by(fyear, reason) %>% count() %>%
   ggplot(aes(x = fyear, y = n, fill = as.factor(reason)))+
   geom_col(position = "fill", alpha = 0.8)+
   scale_fill_manual(values = c("darkslategray4", "peachpuff3", "gray40", "black"))+
   scale_x_continuous(breaks = scales::breaks_width(5))+
   scale_y_continuous(labels = scales::label_percent(), expand = c(0,0))+
   theme_ipsum()+
   theme(legend.position = "bottom", panel.grid.minor = element_blank())+
   # add those labels
   labs(title = "Turnover at the Top: voluntary DEO departures have decreased since 1992",
        subtitle = "CEO departures from S&P 1500 firms, 1992 - 2016",
        x = NULL, y = NULL, fill = NULL,
        caption = "data from Gentry et al. 2021 https://doi.org/10.1002/smj.3278 | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("CEODepartures_plot.png", bg = "transparent", width = 10, height = 6, dpi = 400)
