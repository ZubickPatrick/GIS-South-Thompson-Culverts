# lets make a study area map... 


# need to load in my packages.. 

library(tidyverse)
library(raster)
library(RCurl)
library(sf)
library(maps) 
library(mapdata)
library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
library(ggsn)
library(tidyverse)
library(here)
library(ggmap)
library(cowplot)
library(ggpubr)
library(bcmaps)
library(png)
library(grid)

# lets try to do this myself...cause stupid gpt cant sort it out lolll  

bc.ab = bc_neighbours() 

bc <- bc_bound()

plot(st_geometry(bc))

water5M = watercourses_5M()


# add sites in... 

Compliance_Study_Sites <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-R-Git-CODING/MSc_Compliance_RStudio/Compliance-Study-Sites.csv")

# need to clean up df by removing the doubles @ fortune, campbell

Compliance_Study_Sites$Site <- ifelse(Compliance_Study_Sites$Site %in% c("DFO 2A", "DFO 2B"),
                                      gsub("DFO 2A|DFO 2B", "DFO 2", Compliance_Study_Sites$Site),
                                      Compliance_Study_Sites$Site)

Compliance_Study_Sites$Site <- ifelse(Compliance_Study_Sites$Site %in% c("DFO 24A", "DFO 24B"),
                                      gsub("DFO 24A|DFO 24B", "DFO 24", Compliance_Study_Sites$Site),
                                      Compliance_Study_Sites$Site)

Compliance_Study_Sites$Site <- ifelse(Compliance_Study_Sites$Site %in% c("DFO 4A", "DFO 4B"),
                                      gsub("DFO 4A|DFO 4B", "DFO 4", Compliance_Study_Sites$Site),
                                      Compliance_Study_Sites$Site)

compliance.sites.clean = unique(Compliance_Study_Sites)


#Convert the compliance.sites.clean data frame to an sf object
compliance_sf <- st_as_sf(compliance.sites.clean, coords = c("Long", "Lat"), crs = 4326)

# Transform the coordinates to BC Albers (EPSG code 3005)
compliance_sf_transformed <- st_transform(compliance_sf, 3005)


# Convert the compliance.sites.clean data frame to an sf object
compliance_sf <- st_as_sf(compliance.sites.clean, coords = c("Long", "Lat"), crs = 4326)

# Transform the coordinates to BC Albers (EPSG code 3005)
compliance_sf_transformed <- st_transform(compliance_sf, 3005)

# reproject compliance_sf_transformed to UTM
compliance_sf_transformed <- st_transform(compliance_sf_transformed, 32610)

# create a bounding box around the points in compliance_sf_transformed
bbox <- st_bbox(compliance_sf_transformed)

# crop bc to the extent of the buffered bounding box

# convert bbox to sf object and buffer it
bbox_sf <- st_as_sfc(bbox)
bbox_sf_buffered <- st_buffer(bbox_sf, dist = 10000)

# need to set to the same crs
# Ensure that the spatial reference systems match
bbox_sf_buffered <- st_transform(bbox_sf_buffered, st_crs(bc))

bc_cropped <- st_crop(bc, bbox_sf_buffered)
bc_water_cropped <- st_crop(water5M, bc_cropped)

plot(bc_water_cropped)

# need to plot the basemap... 


# Create the basemap
bc_map <- ggplot() +
  geom_sf(data = bc, fill = "grey85", col = "black", lwd = 0.1) + geom_sf(data = bc_neighbours())+
  coord_sf(datum = NA) + geom_sf(data = watercourses_5M()) +
  geom_sf(data = bbox_sf_buffered, fill = NA, color = "red", size = 1.5)+
  theme_classic()

bc_map

# try to plot the bc cropped

# Create the main map with some stream lines... Probs want majr lakes in bc too...

bc_map_cropped <- ggplot() +
  geom_sf(data = bc_cropped, fill = "grey85", col = "black", lwd = 0.1) +
  coord_sf(datum = NA) +
  geom_sf(data = compliance_sf_transformed, aes(color = remediation_class)) +
  scale_colour_manual(values = c("#ff6700", "#c15ccb", "#057076"))+geom_sf(data = bc_water_cropped, col = "lightblue") +
  theme_classic() +
  theme(legend.position = "none") # remove legend

bc_map_cropped

# looking good so far... 

# i want to have this plot surrounded by  a balck outline rectangle..

str(bc_map_cropped)

# try to add them together 


# add sites in... 

Compliance_Study_Sites <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-R-Git-CODING/MSc_Compliance_RStudio/Compliance-Study-Sites.csv")

# need to clean up df by removing the doubles @ fortune, campbell

Compliance_Study_Sites$Site <- ifelse(Compliance_Study_Sites$Site %in% c("DFO 2A", "DFO 2B"),
                                      gsub("DFO 2A|DFO 2B", "DFO 2", Compliance_Study_Sites$Site),
                                      Compliance_Study_Sites$Site)

Compliance_Study_Sites$Site <- ifelse(Compliance_Study_Sites$Site %in% c("DFO 24A", "DFO 24B"),
                                      gsub("DFO 24A|DFO 24B", "DFO 24", Compliance_Study_Sites$Site),
                                      Compliance_Study_Sites$Site)

Compliance_Study_Sites$Site <- ifelse(Compliance_Study_Sites$Site %in% c("DFO 4A", "DFO 4B"),
                                      gsub("DFO 4A|DFO 4B", "DFO 4", Compliance_Study_Sites$Site),
                                      Compliance_Study_Sites$Site)

compliance.sites.clean = unique(Compliance_Study_Sites)


#Convert the compliance.sites.clean data frame to an sf object
compliance_sf <- st_as_sf(compliance.sites.clean, coords = c("Long", "Lat"), crs = 4326)

# Transform the coordinates to BC Albers (EPSG code 3005)
compliance_sf_transformed <- st_transform(compliance_sf, 3005)


# Convert the compliance.sites.clean data frame to an sf object
compliance_sf <- st_as_sf(compliance.sites.clean, coords = c("Long", "Lat"), crs = 4326)

# Transform the coordinates to BC Albers (EPSG code 3005)
compliance_sf_transformed <- st_transform(compliance_sf, 3005)

# reproject compliance_sf_transformed to UTM
compliance_sf_transformed <- st_transform(compliance_sf_transformed, 32610)

# create a bounding box around the points in compliance_sf_transformed
bbox <- st_bbox(compliance_sf_transformed)

# crop bc to the extent of the buffered bounding box

# convert bbox to sf object and buffer it
bbox_sf <- st_as_sfc(bbox)
bbox_sf_buffered <- st_buffer(bbox_sf, dist = 10000)

# need to set to the same crs
# Ensure that the spatial reference systems match
bbox_sf_buffered <- st_transform(bbox_sf_buffered, st_crs(bc))

bc_cropped <- st_crop(bc.ab, bbox_sf_buffered)
bc_water_cropped <- st_crop(water5M, bc_cropped)

plot(bc_water_cropped)

# need to plot the basemap... 


# Create the basemap
bc_map <- ggplot() +
  geom_sf(data = bc, fill = "grey85", col = "black", lwd = 0.1) + geom_sf(data = bc_neighbours())+
  coord_sf(datum = NA) + geom_sf(data = watercourses_5M()) +
  geom_sf(data = bbox_sf_buffered, fill = NA, color = "red", size = 1.5)+
  theme_classic()

bc_map

# try to plot the bc cropped

# Create the main map with some stream lines... Probs want majr lakes in bc too...

bc_map_cropped <- ggplot() +
  geom_sf(data = bc_cropped, fill = "grey85", col = "black", lwd = 0.1) +
  coord_sf(datum = NA) +
  geom_sf(data = compliance_sf_transformed, aes(color = remediation_class)) +
  scale_colour_manual(values = c("#ff6700", "#c15ccb", "#057076"))+geom_sf(data = bc_water_cropped, col = "darkblue") +
  theme_classic() +
  theme(legend.position = "none") # remove legend

bc_map_cropped

# looking good so far... 

# i want to have this plot surrounded by  a balck outline rectangle..

str(bc_map_cropped)

# try to add them together 

# lets try to add in the lakes...from sockeye shapefile..


# Create the basemap
bc_map <- ggplot() +
  geom_sf(data = bc, fill = "grey85", col = "black", lwd = 0.1) + geom_sf(data = bc_neighbours()) + geom_sf(data = watercourses_15M()) +
  geom_sf(data = bbox_sf_buffered, fill = NA, color = "red", lwd = 1.5)+
  theme_classic()

bc_map

# I would like to add major lakes of bc...

# my sockeye lake shapefile shoudl do that... 

st_crs(shapefile.sox)

shapefile.sox <- st_read("SEL_CU_BOUNDARY_En.shp")
sockeyeUTM <- st_transform(shapefile.sox, crs = 3157) 
st_crs(bcUTM)
bcUTM <- st_transform(bc, crs = 3157) 

ggplot() + geom_sf(data = bcUTM, col = "black", fill = "grey75", lwd = 0.01) + geom_sf(data = sockeyeUTM, fill = "darkblue") +
  theme_bw()


# pretty close but not wuite good enough... would prefer just to have the large lakes... 

lakes <- st_read("DBMBC7HML2_polygon.shp")
lakeutm <- st_transform(lakes, crs = 3157) 
st_crs(lakeutm)

ggplot() + geom_sf(data = bcUTM, col = "black", fill = "grey75", lwd = 0.01) + geom_sf(data = lakeutm, fill = "darkblue") +
  theme_bw()


# need to quickly cut all the alberta stuff lol...

# Use st_intersection() to get only the lakes that intersect with bcUTM
lakeutm_bc <- st_intersection(lakeutm, bcUTM)

# plot again

ggplot() + geom_sf(data = bcUTM, col = "black", fill = "grey75", lwd = 0.01) + geom_sf(data = lakeutm_bc, fill = "darkblue") +
  theme_bw()

# add it all togetherrrrrr...

bcstreams = watercourses_15M()


# Create the basemap stream lines, box of interest, and lakes... 
bc_map <- ggplot() +
  geom_sf(data = bc, fill = "grey85", col = "black", lwd = 0.1) + geom_sf(data = bc_neighbours()) + geom_sf(data = bcstreams,col = "darkblue") + geom_sf(data = lakeutm_bc, fill = "darkblue")+
  geom_sf(data = bbox_sf_buffered, fill = NA, color = "red", lwd = 1.5)+
  theme_classic()

bc_map

# Save the ggplot object as a PNG file
ggsave("bc_map.png", bc_map, width = 10, height = 8, dpi = 300)

#need to crop the lakes 
bc_lakes_cropped <- st_crop(lakes, bc_cropped)
lakeutm.cropped <- st_transform(bc_lakes_cropped, crs = 3157)

# Create the main map with some stream lines... Probs want majr lakes in bc too...

bc_map_cropped <- ggplot() +
  geom_sf(data = bc_cropped, fill = "grey85", col = "black", lwd = 0.1) +
  coord_sf(datum = NA) +
  geom_sf(data = compliance_sf_transformed, aes(color = remediation_class)) +
  scale_colour_manual(values = c("#ff6700", "#c15ccb", "#057076"))+geom_sf(data = bc_water_cropped, col = "darkblue") + geom_sf(data = lakeutm.cropped, fill = "darkblue")+
  theme_classic() +
  theme(legend.position = "none") # remove legend

bc_map_cropped


# need to add scale bar, north arrow, and then paste the full map on the smaller one as an inset.. 

# attach image..

img <- readPNG("bc_map.png")

# Create a grob object for the image
img_grob <- rasterGrob(img, interpolate = TRUE)

# Extract the x and y ranges of the ggplot object
xrange <- layer_scales(bc_map_cropped)$x$range$range
yrange <- layer_scales(bc_map_cropped)$y$range$range

xrange
yrange

# Add the image to the bc_map_cropped ggplot object
bc_map_cropped + annotation_custom(img_grob, xmin = 1238109, xmax = 1518730, ymin = 586155.5, ymax = 1013345.6)

# well the resolution sucks asssssss but its okay I suppose... 

# might try the other way tmrw from the workshop thing!!!
