#how to create maps in R

library(dplyr)
library(plyr)
library(tidyr)
library(plyr)
library(sf)
library(sp)
library(s2)
library(ggplot2)
#library(googleway)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(raster)
library(terra)

remotes::install_github("ropensci/rnaturalearthhires")
install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")


setwd("C:/Users/ks162/Documents/R script")

coords<- read.csv("example_coord.csv")

#Step1: set up simple feature (country, states)

world<- ne_states(returnclass = "sf")

#Step2: Add any overlays (like climate/elevation)

North_America<- rnaturalearth:: ne_countries(continent = "north america", returnclass = "sf")
plot(North_America)

climate_data<- raster:: getData("worldclim" , var="bio", res=2.5)

raster_NA<- raster(North_America, res = 0.01)
values(raster_NA) <- 1:ncell(raster_NA)
raster_NA <- mask(raster_NA, North_America)
plot(raster_NA)
plot(North_America, add=TRUE)
points(coords$latitude, coords$longitude)

library(terra)
raster_rainfall <- climate$bio12

North_America<- rnaturalearth:: ne_countries(continent = "north america", returnclass = "sf")
plot(North_America)

# if using rainfall data (or any other climate data), change the raster file into a df.
# then set values as numeric
# then plot points on your selected area to view: use google maps to create a square plot
# filter(between(x, -128, -57))
# filter(between(y, 49, 16))

#xlim = c(-128, -57), ylim = c(16, 49)
#xlim = c(-110, -70), ylim = c(25, 40)

coord_map<- ggplot() + geom_sf(data = world, fill = NA, color= "black") + 
  coord_sf(xlim = c(-110, -70), ylim = c(25, 40), 
           expand = FALSE) + theme_bw() + 
  geom_point(data = coords, aes(x=longitude, y=latitude))

head(coords)

AGHY_coords<- filter(coords, SpeciesID == "AGHY")

AGHY_coord_map<- ggplot() + geom_sf(data = world, fill = NA, color= "black") + 
  coord_sf(xlim = c(-110, -80), ylim = c(25, 36), 
           expand = FALSE) + 
  geom_point(data = AGHY_coords, aes(x=longitude, y=latitude))

