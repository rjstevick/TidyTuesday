# Friends emotions
# TidyTuesday 2020 week 37
# Rebecca Stevick updated 9/8/2020

# Load libraries ---------------
library(tidyverse)
# download Friends font from https://www.ffonts.net/Friends.font.download, then install on your computer, then run extrafont::font_import()

# Load data --------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-08')

# Analysis and plotting ------

# join the emotions with who said them
friends <- tuesdata$friends
friends_emotions <- tuesdata$friends_emotions %>%
  left_join(friends)

# make a dataframe for each arrow data with the facet included
dataross<-data.frame(x=1,xend=1.3,y=340,yend=303,emotion="Scared")
datajoey<-data.frame(x=1.1,xend=0.82,y=270,yend=210,emotion="Mad")

friends_emotions %>%
  # select only main characters
  filter(speaker %in% c("Ross Geller", "Monica Geller", "Chandler Bing", "Joey Tribbiani", "Rachel Green")) %>% 
  # remove neutral since it's similar for everyone
  filter(emotion != "Neutral") %>% 
  # group by speaker and emotion, then count
  group_by(speaker, emotion) %>% count() %>% 
  # time to plot!
  ggplot(aes(x=emotion, y=n, fill=speaker))+
  # add a column and facet by emotion
  geom_col(position="dodge")+facet_grid(~emotion, scales = "free")+
  # change color scheme
  scale_fill_manual(values=c("#ff4238", "#ffDC00", "#42A2D6", "#9a0006", "#fff580"))+
  # add label and arrow for Ross
  geom_label(data=dataross, aes(x=x,y=y, label = "Ross is the \nmost scared"),inherit.aes=FALSE, alpha=0, hjust = 1, vjust = 0.5, lineheight = 0.8, family="Tahoma", color="white", label.size = NA, size = 4)+
  geom_curve(data=dataross, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE,  color="white", size=0.5, curvature = -0.2, arrow = arrow(length = unit(0.08, "npc")))+
  # add label and arrow for Joey
  geom_label(data=datajoey, aes(x=x,y=y, label = "Joey is the \nleast mad"),inherit.aes=FALSE, alpha=0, hjust = 0.2, vjust = 0, lineheight = 0.8, family="Tahoma", color="white", label.size = NA, size = 4)+
  geom_curve(data=datajoey, aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE,  color="white", size=0.5, curvature = 0.2, arrow = arrow(length = unit(0.08, "npc")))+
  # change up the theme
  theme_minimal()+
  theme(plot.background = element_rect("black"), text=element_text(family="Friends", color="white"),
        axis.text.y = element_text(family="Tahoma", color="white"), plot.caption = element_text(family="Tahoma", color="white", size=11),
        plot.title = element_text(family="Friends", color="white", size=24, hjust=0.5),
        axis.text.x = element_blank(), axis.ticks = element_line(color="black"),
        panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(), 
        strip.text = element_text(family="Friends", color="white", size=12),
        legend.position = "top")+
  # add those labels
  labs(title="The one with the emotional friends",
       caption="data from friends R package (Emil Hvitfeldt) | plot by @rjstevick for #TidyTuesday",
       x=NULL, y="Number of times emotion expressed",fill=NULL)

# Saving -----------------------
ggsave("Friends_plot.png", width = 12, height = 6.5, dpi = 400)
