# US Scooby Doo
# TidyTuesday 2021 week 29
# Rebecca Stevick updated 7/20/2021

# Load libraries -----------------
library(tidyverse)
library(hrbrthemes)

# Load data ----------------------
tuesdata <- tidytuesdayR::tt_load('2021-07-13')
scoobydoo <- tuesdata$scoobydoo

# Analysis and plotting ----------
scoobydoo %>%
   select(index, caught_fred:unmask_scooby) %>% 
   pivot_longer(caught_fred:caught_scooby, names_to="caught", values_to="valuecaught") %>% 
   pivot_longer(captured_fred:captured_scooby, names_to="captured", values_to="valuecaptured") %>% 
   pivot_longer(unmask_fred:unmask_scooby, names_to="unmask", values_to="valuemask") %>% 
   pivot_longer(c(caught, captured, unmask), names_to="type", values_to="charactername") %>% 
   pivot_longer(c(valuecaught, valuecaptured, valuemask)) %>% 
   separate(charactername, into=c("type2", "charactername"), sep="_") %>% 
   group_by(type, charactername) %>% count(value) %>% 
   filter(value == "TRUE") %>% 
   mutate(charactername = str_to_sentence(charactername),
          type = str_to_sentence(type)) %>% 
   # start plotting
   ggplot(aes(x=n, y=charactername, fill=charactername))+
   # make a panel per action type
   facet_grid(.~type)+
   # add segment for lollipop
   geom_segment(aes(x = 0, y = reorder(charactername,-n), xend = n, yend = reorder(charactername,-n)),
                color = "bisque2", alpha = 0.6, lwd = 2)+
   # add point at end of lollipop
   geom_point(aes(color=charactername), size = 5, alpha = 0.9)+
   # edit color palette
   scale_color_manual(values=c("#128a84", "#79af30", "#bb5c37", "#4b0055", "#8e6345"))+
   # change global theme
   theme_ipsum()+
   # edit theme
   theme(legend.position = "none", panel.grid.major.y = element_blank(),
         strip.text = element_text(hjust = 0.5, face = "bold"))+
   # add those labels
   labs(x = NULL, y = NULL, 
        title = "Caught, Captured, Unmasked: \nWho is the best ghost hunter in Mystery Inc?",
        subtitle = "Number of times each character was captured, caught the monster, or unmasked the culprit",
        caption = "data from Kaggle | plot by @rjstevick for #TidyTuesday")

# Saving -------------------------
ggsave("ScoobyDoo_plot.png", bg = "transparent", width = 9, height = 5, dpi = 400)
