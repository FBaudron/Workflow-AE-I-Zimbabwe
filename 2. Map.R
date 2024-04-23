#' ---
#' title: "Maps ALLs"
#' author: "Frédéric Baudron"
#' date: "April 9th, 2024"
#' ---


rm(list=ls())


# installing and loading necessary packages

if (!require('sf')) install.packages("sf")
if (!require('raster')) install.packages("raster")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('ggspatial')) install.packages("ggspatial")
if (!require('ggthemes')) install.packages("ggthemes")
if (!require('grid')) install.packages("grid")


library(sf)
library(raster)
library(ggplot2)
library(ggspatial)
library(ggthemes)
library(grid)



# set your directory

setwd('D:\\Mes Donnees\\2. CIMMYT\\2. Agroeco IDT\\zz. R training\\')


# download country rasters from https://www.diva-gis.org/gdata

ADMIN1 = read_sf("Input files\\zwe_admbnda_adm1_zimstat_ocha_20180911\\zwe_admbnda_adm1_zimstat_ocha_20180911.shp")
ADMIN2 = read_sf("Input files\\zwe_admbnda_adm2_zimstat_ocha_20180911\\zwe_admbnda_adm2_zimstat_ocha_20180911.shp")
ADMIN3 = read_sf("Input files\\zwe_admbnda_adm3_zimstat_ocha_20180911\\zwe_admbnda_adm3_zimstat_ocha_20180911.shp")

ELZIMMSK = raster("Input files\\ZWE_alt\\ZWE_alt.gri")
ELZIMMSK_df = as.data.frame(ELZIMMSK, xy = TRUE)

RIVERS = read_sf("Input files\\ZWE_wat\\ZWE_water_lines_dcw.shp")
LAKES = read_sf("Input files\\ZWE_wat\\ZWE_water_areas_dcw.shp")

MR = subset(ADMIN3, subset = ADM2_EN == "Murehwa")
SITE_MR = subset(MR, subset = ADM3_EN == "4" | ADM3_EN == "27")
WD_MR = st_centroid(MR)
WD_MR = cbind(WD_MR, st_coordinates(st_centroid(WD_MR$geometry)))

MB = subset(ADMIN3, subset = ADM2_EN == "Mbire")
SITE_MB = subset(MB, subset = ADM3_EN == "2" | ADM3_EN == "3")
WD_MB = st_centroid(MB)
WD_MB = cbind(WD_MB, st_coordinates(st_centroid(WD_MB$geometry)))



# MUREHWA-----------------------------------------------------------------------

ggplot() + 
  geom_raster(data = na.omit(ELZIMMSK_df), aes(x = x, y = y, fill = ZWE_alt))


# change color palette

ggplot() + 
  geom_raster(data = na.omit(ELZIMMSK_df), aes(x = x, y = y, fill = ZWE_alt)) +
  scale_fill_distiller(palette = "RdYlGn")

# zoom on Murehwa District

ggplot() + 
  geom_raster(data = na.omit(ELZIMMSK_df), aes(x = x, y = y, fill = ZWE_alt)) +
  scale_fill_distiller(palette = "RdYlGn") +
  coord_sf(xlim = c(31.5, 32.25), ylim = c(-18.25, -17.35)) 


# set limits for elevation

ggplot() + 
  geom_raster(data = na.omit(ELZIMMSK_df), aes(x = x, y = y, fill = ZWE_alt)) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(900, 1800)) +
  coord_sf(xlim = c(31.5, 32.25), ylim = c(-18.25, -17.35))


# add ward boundaries

ggplot() + 
  geom_raster(data = na.omit(ELZIMMSK_df), aes(x = x, y = y, fill = ZWE_alt)) +
  geom_sf(data = MR, fill = NA, linewidth = 1, color = "black") +
  scale_fill_distiller(palette = "RdYlGn", limits = c(900, 1800)) +
  coord_sf(xlim = c(31.5, 32.25), ylim = c(-18.25, -17.35)) 


# add rivers and lakes

ggplot() + 
  geom_raster(data = na.omit(ELZIMMSK_df), aes(x = x, y = y, fill = ZWE_alt)) +
  geom_sf(data = RIVERS, color = "blue") +
  geom_sf(data = LAKES, color = "blue", fill = "blue") +
  geom_sf(data = MR, fill = NA, linewidth = 1, color = "black") +
  scale_fill_distiller(palette = "RdYlGn", limits = c(900, 1800)) +
  coord_sf(xlim = c(31.5, 32.25), ylim = c(-18.25, -17.35)) 


# adding ward sites

ggplot() + 
  geom_raster(data = na.omit(ELZIMMSK_df), aes(x = x, y = y, fill = ZWE_alt)) +
  geom_sf(data = RIVERS, color = "blue") +
  geom_sf(data = LAKES, color = "blue", fill = "blue") +
  geom_sf(data = SITE_MR, fill = "purple", linewidth = 1, color = NA, alpha = 0.2) +
  geom_sf(data = MR, fill = NA, linewidth = 1, color = "black") +
  scale_fill_distiller(palette = "RdYlGn", limits = c(900, 1800)) +
  coord_sf(xlim = c(31.5, 32.25), ylim = c(-18.25, -17.35)) 


# adding ward numbers

ggplot() + 
  geom_raster(data = na.omit(ELZIMMSK_df), aes(x = x, y = y, fill = ZWE_alt)) +
  geom_sf(data = RIVERS, color = "blue") +
  geom_sf(data = LAKES, color = "blue", fill = "blue") +
  geom_sf(data = SITE_MR, fill = "purple", linewidth = 1, color = NA, alpha = 0.2) +
  geom_sf(data = MR, fill = NA, linewidth = 1, color = "black") +
  geom_text(data= WD_MR, aes(x = X , y = Y + 0.018, label = ADM3_EN),
            color = "black", fontface = "bold.italic", size = 3, check_overlap = FALSE) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(900, 1800)) +
  coord_sf(xlim = c(31.5, 32.25), ylim = c(-18.25, -17.35)) 


# adding plot title and changing axis titles

ggplot() + ggtitle("Murehwa") +
  geom_raster(data = na.omit(ELZIMMSK_df), aes(x = x, y = y, fill = ZWE_alt)) +
  geom_sf(data = RIVERS, color = "blue") +
  geom_sf(data = LAKES, color = "blue", fill = "blue") +
  geom_sf(data = SITE_MR, fill = "purple", linewidth = 1, color = NA, alpha = 0.2) +
  geom_sf(data = MR, fill = NA, linewidth = 1, color = "black") +
  geom_text(data= WD_MR, aes(x = X , y = Y + 0.018, label = ADM3_EN),
            color = "black", fontface = "bold.italic", size = 3, check_overlap = FALSE) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(900, 1800)) +
  xlab("") + ylab("") +
  coord_sf(xlim = c(31.5, 32.25), ylim = c(-18.25, -17.35)) 


# change theme and legend title

ggplot() + ggtitle("Murehwa") +
  geom_raster(data = na.omit(ELZIMMSK_df), aes(x = x, y = y, fill = ZWE_alt)) +
  geom_sf(data = RIVERS, color = "blue") +
  geom_sf(data = LAKES, color = "blue", fill = "blue") +
  geom_sf(data = SITE_MR, fill = "purple", linewidth = 1, color = NA, alpha = 0.2) +
  geom_sf(data = MR, fill = NA, linewidth = 1, color = "black") +
  geom_text(data= WD_MR, aes(x = X , y = Y + 0.018, label = ADM3_EN),
            color = "black", fontface = "bold.italic", size = 3, check_overlap = FALSE) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(900, 1800)) +
  theme_few() + xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(0, 0, 10, 0)),
        legend.title = element_text(margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(angle = 90, hjust=0.5, size = 12),
        axis.text.x = element_text(size = 12))+ 
  coord_sf(xlim = c(31.5, 32.25), ylim = c(-18.25, -17.35)) +
  labs(fill = "Elevation (m.a.s.l.)")


# add scale and North arrow

ggplot() + ggtitle("Murehwa") +
  geom_raster(data = na.omit(ELZIMMSK_df), aes(x = x, y = y, fill = ZWE_alt)) +
  geom_sf(data = RIVERS, color = "blue") +
  geom_sf(data = LAKES, color = "blue", fill = "blue") +
  geom_sf(data = SITE_MR, fill = "purple", linewidth = 1, color = NA, alpha = 0.2) +
  geom_sf(data = MR, fill = NA, linewidth = 1, color = "black") +
  geom_text(data= WD_MR, aes(x = X , y = Y + 0.018, label = ADM3_EN),
            color = "black", fontface = "bold.italic", size = 3, check_overlap = FALSE) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(900, 1800)) +
  theme_few() + xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(0, 0, 10, 0)),
        legend.title = element_text(margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(angle = 90, hjust=0.5, size = 12),
        axis.text.x = element_text(size = 12))+ 
  coord_sf(xlim = c(31.5, 32.25), ylim = c(-18.25, -17.35)) +
  labs(fill = "Elevation (m.a.s.l.)") +
  annotation_scale(location = "tr", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering)


p1 = ggplot() + ggtitle("Murehwa") +
  geom_raster(data = na.omit(ELZIMMSK_df), aes(x = x, y = y, fill = ZWE_alt)) +
  geom_sf(data = RIVERS, color = "blue") +
  geom_sf(data = LAKES, color = "blue", fill = "blue") +
  geom_sf(data = SITE_MR, fill = "purple", linewidth = 1, color = NA, alpha = 0.2) +
  geom_sf(data = MR, fill = NA, linewidth = 1, color = "black") +
  geom_text(data= WD_MR, aes(x = X , y = Y + 0.018, label = ADM3_EN),
            color = "black", fontface = "bold.italic", size = 3, check_overlap = FALSE) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(900, 1800)) +
  theme_few() + xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(0, 0, 10, 0)),
        legend.title = element_text(margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(angle = 90, hjust=0.5, size = 12),
        axis.text.x = element_text(size = 12))+ 
  coord_sf(xlim = c(31.5, 32.25), ylim = c(-18.25, -17.35)) +
  labs(fill = "Elevation (m.a.s.l.)") +
  annotation_scale(location = "tr", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering)

p1

pol1 <- data.frame(xmin = 31.5, xmax = 32.25, ymin = -18.25, ymax = -17.35)


p2 <- ggplot() + 
  geom_sf(data = ADMIN1, fill = "grey95", linewidth = 0.5, color = "black") + 
  geom_rect(data = pol1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0, colour = "red", linewidth = 1, linetype = 1) + 
  theme_few() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) 

p2


png(file = "Murehwa ALLs.png", w = 2600, h = 2500, res = 300)

grid.newpage()

v1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2 <- viewport(width = 0.15, height = 0.15, x = 0.18, y = 0.15) #plot area for the inset map
print(p1,vp = v1) 
print(p2,vp = v2)
dev.off()


# MBIRE-------------------------------------------------------------------------

p3 <- ggplot() + ggtitle("Mbire") +
  geom_raster(data = na.omit(ELZIMMSK_df), aes(x = x, y = y, fill = ZWE_alt)) +
  geom_sf(data = RIVERS, color = "blue") +
  geom_sf(data = LAKES, color = "blue", fill = "blue") +
  geom_sf(data = SITE_MB, fill = "purple", linewidth = 1, color = NA, alpha = 0.2) +
  geom_sf(data = MB, fill = NA, linewidth = 1, color = "black") +
  geom_text(data= WD_MB, aes(x = X, y = Y, label = ADM3_EN),
            color = "black", fontface = "bold.italic", size = 3, check_overlap = FALSE) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(300, 1450)) +
  theme_few() + xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(0, 0, 10, 0)),
        legend.title = element_text(margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(angle = 90, hjust=0.5, size = 12),
        axis.text.x = element_text(size = 12))+ 
  coord_sf(xlim = c(30.02, 31.15), ylim = c(-16.42, -15.62)) +
  labs(fill = "Elevation (m.a.s.l.)") +
  annotation_scale(location = "tr", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering)

# p3

pol2 <- data.frame(xmin = 30.02, xmax = 31.15, ymin = -16.42, ymax = -15.62)

p4 <- ggplot() + 
  geom_sf(data = ADMIN1, fill = "grey95", linewidth = 0.5, color = "black") + 
  geom_rect(data = pol2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0, colour = "red", linewidth = 1, linetype = 1) + 
  theme_few() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
# p4

png(file = "Mbire ALLs.png", w = 3600, h = 2300, res = 300)

grid.newpage()

v1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2 <- viewport(width = 0.15, height = 0.15, x = 0.13, y = 0.85) #plot area for the inset map
print(p3, vp = v1) 
print(p4, vp = v2)
dev.off()


