#####################################################
### Extracting SST from raster to points function ###
###              27/01/23                         ###
#####################################################

#NB MUST USE THE SST_RASTER_GH FILES THAT HAVE _SST IN THE FILE NAME

#modified this from my 05_extract_AMSR2 

##SST downloaded from: https://cmr.earthdata.nasa.gov/virtual-directory/collections/C2036880657-POCLOUD/temporal/2022

#filepath to SST rasters is now: 
# 'C:/Users/emhon/OneDrive/Documents/R_Projects/SNPE_Tracking_2022/Environmental_Data/SST/SST_Raster_GH/'

#libraries
library(tidyverse)
library(lubridate)
library(tibble)
library(raster)

rm(list=ls())

path <- 'Environmental_Data/Sea_Surface_Temperature/SST_Raster_GH/'

list.files(path)
df <- read.csv("Trip_Covariates/05_Trips_with_SIC.csv")

df <- df %>% as_tibble()
#df <- df %>% mutate(datetime = lubridate::dmy_hm(datetime))
df <- df %>% mutate(datetime = lubridate::dmy_hm(RTIME))

head(df)
nrow(df)


###the function

extract_SST <- function(df, path){
  
  ## build a file-db
  files <- tibble(fullname = list.files(normalizePath(path), full.names = TRUE, pattern = "tiff"), 
                  date = as.POSIXct(strptime(basename(fullname), "%Y%m%d_SST.tiff", tz = "UTC")))
  
  ## map our data to this file-db (we might have multiple points per file)
  df$fullname <- files$fullname[findInterval(df$datetime, files$date)]
  
  (rdummy <- raster(df$fullname[1]))
  
  ## project the query points
  df[c("X", "Y")] <- as_tibble(rgdal::project(as.matrix(df[c("LON", "LAT")]), projection(rdummy)))
  
  ## set up progress bar based on number of files that will be loaded
  pb <- progress_estimated(length(unique(df$fullname)))
  
  
  ## now, extract per file 
  
  df <- purrr::map_df(split(df, df$fullname)[unique(df$fullname)], 
                      function(.x) {
                        pb$tick()$print()
                        .x["SST"] <- raster::extract(raster(.x$fullname[1]), as.matrix(.x[c("X", "Y")]))
                        .x
                      })
  
  
  #need to work out how sst is stored
  
  
  # if animal locations are further north than ice raster extent set concentration to 0
  # df <- df %>% mutate(concentration = if_else(Y > raster::extent(rdummy)[4], 0, concentration))
  
  # #0 is ocean; 2510 pole hole; 2530 coast line; 2540 land; 2550 missing
  # 0-1000 so divide by 10 to get percentage
  
  # df$SST <- df$SST*10 #<-this puts it at 2.xxx degrees 
  hist(df$SST)
  
  # check order of dataframe
  df <- df %>% arrange(TripID, datetime)
  df <- df %>% dplyr::select(-c("fullname", "X", "Y"))
  
  return(df)
}

SST_funct_check <- extract_SST(df, path)


#write.csv(SST_funct_check,"C:/Users/emhon/OneDrive/Documents/R_Projects/SNPE_Tracking_2022/Analyses/SST_19_10.csv", row.names = FALSE)



view(SST_funct_check)
min(SST_funct_check$SST)
max(SST_funct_check$SST)
mean(SST_funct_check$SST)

#NEED TO DROP NAs

nrow(df)
df <- df %>% drop_na()
nrow(df)


