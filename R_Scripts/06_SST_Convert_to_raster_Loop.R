#####################
###Extracting SST ###
###   27/01/23    ###
#####################
#THIS CONTAINS OLD FILE PATHS AS I DIDN'T RE RUN IT 27/01/23 AS I ALREADY HAD THE RASTERS

##SST downloaded from: https://cmr.earthdata.nasa.gov/virtual-directory/collections/C2036880657-POCLOUD/temporal/2022
rm(list = ls())
###write a loop to convert all NC files in path to rasters
#libraries
library(ncdf4)
library(raster)

#test format of files by opening NC of SST
check <- ncdf4::nc_open("Environmental_Data/SST/GHRSST_Level_4/20220113090000-JPL-L4_GHRSST-SSTfnd-MUR25-GLOB-v02.0-fv04.2.nc")

#look at what variables it contains
names(check$var)

#chose "analysed_sst" as the variable

#convert to raster with variable specified
#variable = sst
r <- raster("Environmental_Data/SST/GHRSST_Level_4/20220113090000-JPL-L4_GHRSST-SSTfnd-MUR25-GLOB-v02.0-fv04.2.nc",   varname = "analysed_sst")

#variable = sea ice fraction
t <- raster("Environmental_Data/SST/GHRSST_Level_4/20220113090000-JPL-L4_GHRSST-SSTfnd-MUR25-GLOB-v02.0-fv04.2.nc",   varname = "sea_ice_fraction")


#view
image(r)
image(t)

####loop to extract rasters

## Input directory
dir.nc <- 'C:/Users/emhon/OneDrive/Documents/R_Projects/SNPE_Tracking_2022/Environmental_Data/SST/GHRSST_Level_4/'

list.files(dir.nc)
files.nc <- list.files(dir.nc, full.names = T, recursive = T)

## Output directory
dir.output <- 'C:/Users/emhon/OneDrive/Documents/R_Projects/SNPE_Tracking_2022/Environmental_Data/SST/SST_Raster_GH/'


#issue is in the loop itself - the files are outputted with the number of 'i' as the title. 
#need to edit what the file name is called - just want to retain the date

#works but gives 'i' as file name
for (i in 1:length(files.nc)) {
  r.nc <- raster(files.nc[i])
  writeRaster(r.nc, paste(dir.output, i, '.tiff', sep = ''), format = 'GTiff', overwrite = T)
}


#modify what variable is extracted - this section still give 'i' as file name
for (i in 1:length(files.nc)) {
  r.nc <- raster(files.nc[i], varname="analysed_sst")
  writeRaster(r.nc, paste(dir.output, substr(files.nc[i],1,8), '.tiff', sep = ''), format = 'GTiff', overwrite = T)
}

#ewan code - does somthing funny to the filepath
for (i in 1:length(files.nc)) {
  r.nc <- raster(files.nc[i])
  writeRaster(r.nc, paste(dir.output, substr(files.nc[i],1,8), '.tiff', sep = ''), format = 'GTiff', overwrite = T)
}


###james code -THIS WORKS- I added the '_SST' bit
list.files(dir.nc)
filenames <- list.files(dir.nc)
what_i_want <- substr(filenames, 1, 8)


for (i in 1:length(files.nc)){
  r.nc <- raster(files.nc[i])
  writeRaster(r.nc, paste(dir.output, what_i_want[i], '_SST.tiff', sep = ''),
              format = 'GTiff', overwrite = T)
}

#check
raster_check <-raster('C:/Users/emhon/OneDrive/Documents/R_Projects/SNPE_Tracking_2022/Environmental_Data/SST/SST_Raster_GH/20220123.tiff')

#filepath to SST rasters is now: 
# 'C:/Users/emhon/OneDrive/Documents/R_Projects/SNPE_Tracking_2022/Environmental_Data/SST/SST_Raster_GH/'


#thing from reddit to check source
dput(files.nc)

#end


