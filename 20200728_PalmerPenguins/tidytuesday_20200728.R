# Palmer Penguins
# TidyTuesday 2020 week 31
# Rebecca Stevick updated 7/28/2020

# Load libraries
library(tidyverse)
library(hrbrthemes)
library(ggtext)
library(ggimage)

# Load data
library(palmerpenguins)

# make dataframe of image links, make sure the facet variable is the same
images<-data.frame(species=c("Adelie","Chinstrap","Gentoo"),
                   image=c("https://www.nationalgeographic.com/content/dam/animals/thumbs/rights-exempt/birds/a/adelie-penguin_thumb.jpg",
                           "https://www.nationalgeographic.com/content/dam/animals/thumbs/rights-exempt/birds/c/chinstrap-penguin.jpg",
                           "https://www.nationalgeographic.com/content/dam/animals/thumbs/rights-exempt/birds/g/gentoo-penguin_thumb.ngsversion.1489602603532.adapt.1900.1.JPG"))

# Let's get piping!
penguins %>%
  drop_na(sex) %>%
  # start ggplot with body mass on x-axis
  ggplot(aes(x=body_mass_g))+
  # add penguin images to background
  geom_image(data=images, aes(x=4550, y=6.5, image=image),
             size = 1, by="height")+
  # add histogram, colored by penguin sex
  geom_histogram(aes(fill = sex), color=alpha("white",0.3),
                 alpha = 0.6, position = "identity") +
  # facet/new panel for each species
  facet_grid(.~species)+
  # change bar colors
  scale_fill_manual(values=c("darkmagenta","skyblue2")) +
  # remove extra white space
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(breaks=seq(0,14, by=3), expand = c(0,0))+
  # make sure the photos don't warp
  scale_size_identity()+
  # change theme
  theme_ipsum()+
  theme(legend.position="none", strip.text=element_text(face="bold"),
        plot.subtitle=element_markdown(lineheight = 0.5),
        plot.title = element_text(family="Amaranth"),
        axis.ticks.x=element_line(inherit.blank=FALSE),axis.ticks.y=element_line(inherit.blank=FALSE)) +
  # add labels
  labs(x="Body mass (grams)", y="Number of penguins", shape=NULL,  color=NULL,
       title= "Size distributions of Palmer Penguin species",
       subtitle="In all 3 species surveyed, <span style='color:darkmagenta;'>**female penguins**</span> weigh less than <span style='color:skyblue2;'>**male penguins**</span>.",
       caption = "data from Gorman, Williams & Fraser (2014). doi.org/10.1371/journal.pone.0090081
       photos from National Geographic photo ark (rights exempt, links in code) | plot by @rjstevick for #TidyTuesday")

# Saving -----------------------------
ggsave("PalmerPenguins_plot.png", bg="transparent", width = 8, height = 5, dpi=400)
