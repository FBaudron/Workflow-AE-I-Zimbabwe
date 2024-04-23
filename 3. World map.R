#' ---
#' title: "Global map"
#' author: "Frédéric Baudron"
#' date: "April 9th, 2024"
#' ---


rm(list=ls())


setwd('D:\\Mes Donnees\\2. CIMMYT\\2. Agroeco IDT\\zz. R training\\')


if (!require('ggplot2')) install.packages("ggplot2")
if (!require('ggthemes')) install.packages("ggthemes")
if (!require('spData')) install.packages("spData")
if (!require('sf')) install.packages("sf")


library(ggplot2)
library(ggthemes)
library(spData)
library(sf)


# download world country polygons from spData

data(world)


# remove Antartica

aei <- subset(world, continent != "Antarctica")


# add a variable 'Value' as a row of zeros
aei$Value = rep("0", nrow(aei))


# in Value, replace zeros by '1' for countries of interest

aei$Value = ifelse(aei$name_long == "Burkina Faso" |
                     aei$name_long == "India" |
                     aei$name_long == "Kenya" |
                     aei$name_long == "Lao PDR" |
                     aei$name_long == "Peru" |
                     aei$name_long == "Senegal" |
                     aei$name_long == "Tunisia" |
                     aei$name_long == "Zimbabwe",
                   1, 0)


# choose a projection (I like Mollweide)
# also see https://proj.org/operations/projections/index.html for various projections

aei <- 
  st_cast(aei, 'MULTIPOLYGON') %>%
  st_transform(crs = "+proj=moll")


# transform the variable Value into a factorial one (numeric by default)

aei$Value = as.factor(aei$Value)


# plot

ggplot(aei) +
  geom_sf(aes(fill = Value), size = 0.2, color = "grey90") +
  scale_fill_manual(values=c("grey10", "springgreen")) + 
  labs(title = "Geographic scope of the One CGIAR 'Agroecology Initiative'")+ 
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey10", color = NA),
        plot.title = element_text(color = "white", size = 22, face = "bold", hjust = 0.5, margin = margin(0, 0, 20, 0)),
        plot.margin = margin(20, 0, 0, 0))


# save

ggsave("Map geographic scope.jpeg", units="cm", width = 40, height = 20, dpi = 320)

