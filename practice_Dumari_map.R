#dumari map project

library(tidyverse)
library(sf)
library(terra)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(readxl)
library(rnaturalearthhires)
library(raster)

install.packages("remotes")

remotes::install_github("ropensci/rnaturalearthhires")
install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")


setwd("C:/Users/ks162/Downloads")

location_data<- read.csv("natural_pop_locations.csv")
endo_data<- read.csv("endo_scores.csv")


#cleaning datasets
endo_data$SiteID <- as.factor(endo_data$SiteID)
endo_data<- filter(endo_data, seeds.scored != " " & seeds.scored != "-")
endo_data<- filter(endo_data, seeds.eplus != " " & seeds.eplus != "-")

endo_data$seeds.eplus <- as.numeric(endo_data$seeds.eplus)
endo_data$seeds.scored <- as.numeric(endo_data$seeds.scored)

grouped_data <- endo_data %>% 
  filter(SpeciesID == "AGHY") %>% 
  group_by(SiteID) %>%
  summarise(endo_avg = mean((seeds.eplus)/(seeds.scored)), sample_size = sum(seeds.scored))


location_data$SiteID <- as.factor(location_data$SiteID)

AGHY_loc <- location_data %>% 
  filter(SpeciesID == "AGHY") 
AGHY_loc$SiteID <- as.factor(AGHY_loc$SiteID)
AGHY_loc <- dplyr:: rename(AGHY_loc, lat = latitude..dd.)
AGHY_loc <- dplyr:: rename(AGHY_loc, long = longitude..dd.)



#establishing maps and layers and rasters
World <- ne_states(returnclass = "sf")


climate_data<- raster:: getData("worldclim", var="bio", res = 2.5)
climate<- as.data.frame(climate_data, xy = TRUE) %>% drop_na()

rainfall<- climate %>% dplyr:: filter(between(x, -102, -87))
rainfall<- climate %>% dplyr:: filter(between(y, 25, 36))

#color gradient for precipitation

colors1<-c("#852154","#daa881","#f7f3d6","#61c1a1","#000c3e")


#legend
Legend = expression(atop(atop(textstyle(bold("Annual")),
                              atop(textstyle(bold("Precipitation")),
                                   atop(textstyle("(kg m"^-2*")")
                                   )))))
#precip map 1970-2000
AGHY_map <- ggplot() + geom_tile(data=rainfall, aes(x=x,y=y, fill=bio12)) +
  scale_fill_gradientn(name=Legend, colors = colors1,
                       limits=c(0, 2000), values  = scales::rescale(c(0, 500, 1000, 1500, 2000), from = c(0, 2000)))+
  geom_sf(data = World, fill = NA, color = "black") +
  coord_sf(xlim = c(-102, -87), ylim = c(25, 36), expand = FALSE) +
  geom_point(data = AGHY_loc, aes(x=as.numeric(long), y =as.numeric(lat))) +
  ggtitle("Southern US Endophyte Prevelance" ) + theme(plot.title = element_text(hjust = 0.5))
AGHY_map

#endo presence map + endo presence graident

#colors2<-c("#833ab4", "#c52a62", "#fd1d1d", "#fd582d", "#fcb045")
#5D3A9B, #E66100
#scale_color_gradient(low = "#5d3a9b", high = "#e66100") 
#scale_color_gradient2(low = "#833ab4", mid = "#fd1d1d", high = "#e5ff56"
#(low = "yellow", high = "blue")

merged_data<- merge(location_data, grouped_data)

AGHY_endo <- merged_data %>% 
  filter(SpeciesID == "AGHY")
AGHY_endo <- dplyr:: rename(AGHY_endo, lat = latitude..dd.)
AGHY_endo <- dplyr:: rename(AGHY_endo, long = longitude..dd.)

AGHY_endo

AGHY_map2 <- ggplot() + geom_tile(data=rainfall, aes(x=x,y=y, fill=bio12)) +
  scale_fill_gradientn(name=Legend, colors = colors1,
                       limits=c(0, 2000), values  = scales::rescale(c(0, 500, 1000, 1500, 2000), from = c(0, 2000)))+
  geom_sf(data = World, fill = NA, color = "black") +
  coord_sf(xlim = c(-102, -87), ylim = c(26, 36), expand = FALSE) +
  geom_point(data = AGHY_endo, aes(x=long, y =lat, color = endo_avg, size = sample_size)) +
  scale_color_gradient(low = "yellow", high = "blue") +
  geom_jitter() + xlab("Longitude") + ylab("Lattitude") + labs(color = "Presence Percentage") +
  ggtitle("Southern US Endophyte Prevelance" ) + theme(plot.title = element_text(hjust = 0.5))
AGHY_map2

#make sure averages and sample sizes are all correct before mapping points
#need to merge combined_data with location_data and filter for ALL the correct AGHY from endo_data


