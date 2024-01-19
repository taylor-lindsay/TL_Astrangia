# Title: AP_Studies_Map.R
# Author: Taylor Lindsay
# Date: 1.5.24
# Input files:
#  study location dataset 
# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html 
# Output files: 
#  map? 
# Notes 
# Libraries ---------------------------------------------------------------

# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
#install.packages(c("maps", "mapdata"))


library(tidyverse)
library(ggplot2)
theme_set(theme_bw()) # set theme 
library("sf") # required for all maps 
library("rnaturalearth") # provides a map of countries of the entire world
library("rnaturalearthdata") # data for map making 
library("ggspatial") # used for n arrow and scale bar 
library(maps) 
library(mapdata)

# Tutorial  -------------------------------------------------------------------

#read csv of AP data
coords <- read.csv('~/Desktop/AP_studies_map.csv')

# pull country data and chose the scale, set the class 
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world) # check class 

# draw a map of the world 
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +   # axes labels 
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)")) # title and n countries 

# map color 

#basic colors 
ggplot(data = world) + 
  geom_sf(color = "red", fill = "lightgreen")

# countries by a value 
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# change projection (see link for other ways to do this)
ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

# change extent to new england 
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-69.301369, -74.388038), ylim = c(40.127523, 43.188235), expand = FALSE)

# add scale bar and n arrow 
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-69.301369, -74.388038), ylim = c(40.127523, 43.188235), expand = FALSE)

# see link for adding country names 

# whole map of all studies
ggplot(data = world) +
  geom_sf() +
  geom_point(data=coords, aes(x=long, y=lat)) + 
  annotation_scale(location = "bl", width_hint = 0.3) +
  #annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-90, 0), ylim = c(20, 55), expand = FALSE)

#states data 
sa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

# map of just NB
map_ri <- ggplot(data = world) +
  geom_sf() +
  geom_point(data=coords, aes(x=long, y=lat, color=paper, shape=type), size=2) + 
  scale_shape_manual(values=c(1, 3))+
  annotation_scale(location = "tl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-71.697289,-70.475060), ylim = c(41.946995, 41.343583), expand = FALSE)
map_ri
# this map is decent but low resolution for the RI data 

#left
#-71.697289
#right
#-70.475060

#top 
#41.946995
#bottom 
#41.343583

# save
ggsave("AP_map_ri.jpg", plot = map_ri, path = '~/Desktop/')

