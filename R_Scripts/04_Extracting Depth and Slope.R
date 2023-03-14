##################################
### Extracting Depth and Slope ###
###         26_01_2023         ###
##################################




rm(list=ls())

#open libraries

library(raster)
library(sp)
library(tibble)
library(terra)
library(tidyverse)

#path for where i
pathC <- 'Environmental_Data/Bathymetry/Geotiff/'

dfbath <- read.csv("Processed_Trips/03_Trimmed_Trips.csv")

###Reading in the raster files from https://doi.pangaea.de/10.1594/PANGAEA.937574?format=html#download

BATHRGB <- raster(paste0(pathC, "IBCSO_v2_ice_surface_RGB.tif"))

BATHWGS84 <- raster(paste0(pathC,"IBCSO_v2_ice_surface_WGS84.tif"))

SLOPE <- raster(paste0(pathC,"depth_slope.tif"))

###Making the slope raster
#this was taking too long 27/01/23 - so quit and used previously made version. Code is from Ewan. 
#depth_slope = terrain(BATHWGS84, opt='slope', unit='radians', neighbors=8)
plot(SLOPE)
#writeRaster(depth_slope, paste(path.out.cov, 'depth_slope.tif', sep=''), overwrite=TRUE)

#writeRaster(depth_slope, paste("Environmental_Data/Bathymetry/", 'depth_slope.tif', sep=''), overwrite=TRUE)

#slope <- raster(paste0(pathC,"depth_slope.tif"))

image(BATHRGB)
image(BATHWGS84)
image(slope)


col <- terrain.colors(5)

image(BATHRGB, col=col)

df <- as.data.frame(dfbath)

dfcoord <- cbind(df$LON,df$LAT)

spbath <- SpatialPoints(dfcoord)

#set the CRS
crsRef <- CRS('+proj=longlat +datum=WGS84')

spbath <- SpatialPoints(spbath, proj4string = crsRef)

#extract raster values at points
#this reads in as a value rather than a df

Bath_Depth <- terra::extract(BATHWGS84, spbath)

Extracted_bath_no_columnRGB <- terra::extract(BATHRGB, spbath)

Bath_Slope <- terra::extract(SLOPE, spbath)

head(Bath_Depth)
head(Extracted_Slope)
hist(Extracted_Slope)
hist(Bath_Slope)
max(Bath_Depth)

#make df
Bath_Depth <- as.data.frame(Extracted_bath_no_column)

#Extracted_bath_no_columnRGB <- as.data.frame(Extracted_bath_no_columnRGB)

Bath_Slope <- as.data.frame(Extracted_Slope)

#add columns to main df
Bath_Merged <- cbind(dfbath, Bath_Depth, Bath_Slope)

#filtering to remove points recorded as overland

max(Bath_Filtered$Bath_Depth)

Bath_Filtered <- Bath_Merged %>% filter(Bath_Merged$Bath_Depth < 1)

hist(Bath_Merged$Bath_Depth)
hist(Bath_Filtered$Bath_Depth)

nrow(Bath_Filtered)
nrow(Bath_Merged)
max(Bath_Merged$Bath_Depth)

#102 locs removed as they were over 1 m elevation - not over water


#need to check what values for depth are greater than 0 and remove locs from depth and slope dfs

#write csv now with depth and slope column
write.csv(Bath_Filtered, "Trip_Covariates/04_Trips_with_Depth_Slope.csv")

