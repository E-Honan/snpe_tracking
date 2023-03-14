######################################################
### Extract  AMSR2 sea ice data to points function ###
###   Modified from James' Grecians Harp Pup       ###
###                  27/01/23                      ###
######################################################

library(tidyverse)
library(lubridate)
library(tibble)
library(raster)


path <- 'Environmental_Data/Sea_Ice/AMSR2/'

df <- read.csv('Trip_Covariates/04_Trips_with_Depth_Slope.csv')


df <- df %>% as_tibble()
df <- df %>% mutate(datetime = lubridate::dmy_hm(RTIME))

nrow(df)
df = df[df$ACC != 9999.999,]
nrow(df)

#asi-AMSR2-s3125-20220105-v5.4

extract_AMSR2 <- function(df, path){
  
  ## build a file-db
  files <- tibble(fullname = list.files(normalizePath(path), full.names = TRUE, pattern = "tif$"), 
                  date = as.POSIXct(strptime(basename(fullname), "asi-AMSR2-s3125-%Y%m%d-v5.4", tz = "UTC")))
  
  ## map our data to this file-db (we might have multiple points per file)
  df$fullname <- files$fullname[findInterval(df$datetime, files$date)]
  glimpse(df)
  ## set up progress bar based on number of files that will be loaded
  pb <- progress_estimated(length(unique(df$fullname)))
  
  (rdummy <- raster(df$fullname[1]))
  
  ## project the query points
  df[c("X", "Y")] <- as_tibble(rgdal::project(as.matrix(df[c("LON", "LAT")]), projection(rdummy)))
  
  ## now, extract per file 
  
  df <- purrr::map_df(split(df, df$fullname)[unique(df$fullname)], 
                      function(.x) {
                        pb$tick()$print()
                        .x["concentration"] <- raster::extract(raster(.x$fullname[1]), as.matrix(.x[c("X", "Y")]))
                        .x
                      })
  
  # check order of dataframe
  df <- df %>% arrange(TripID, datetime)
  df <- df %>% dplyr::select(-c("fullname", "X", "Y"))
  
  return(df)
}

#remove land
#must remove values over 100