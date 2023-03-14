###################################
### Splitting Tracks into Trips ###
###          26_01_2023         ###
###################################


###Splitting the tracks from each logger into individual trips using Steffen Oppels Track2kba package
###Splitting both sites in the same script but with seperate csv files


rm(list=ls())
 
install.packages("amt")

library(ggplot2)
library(remotes)
library(track2KBA)
library(lubridate) 
#library(amt)
library(dplyr)
library(tidyverse)


#Manually added UT028 - it's the first track of UT058

UTDataForMetrics <- read.csv("Processed_Trips/01_POS_Extracted_UT.csv")
SVDataForMetrics <- read.csv("Processed_Trips/01_POS_Extracted_SV.csv")


##########################
#####Utsteinen Tracks#####
##########################

#Remove erroneous locations (where both Lat and lon are zero)
nrow(UTDataForMetrics)
UTDataForMetrics = UTDataForMetrics[UTDataForMetrics$ACC != 9999.999,]
nrow(UTDataForMetrics)

#check all observations are complete (code from amt package vignette)

all(complete.cases(UTDataForMetrics)) # no action required

### Formatting data and time columns

#want to split RTIME into 2 columns, one time and one date. 

#new datatime column formatted as a datetime
UTDataForMetrics$DateTime <- UTDataForMetrics$RTIME

#formatted as a timestamp
UTDataForMetrics <- UTDataForMetrics %>% mutate(DateTime = lubridate::dmy_hm(DateTime))

#splitting into data and time was done in excel


dat1UT <- as_tibble(UTDataForMetrics) %>% dplyr::select (LON = LON, LAT = LAT,
                                                         TIME = TIME, DATE = DATE, id=BIRDID)

head(dat1UT)


#Now using the github example for track2iba

tibble(dat1UT)

#converted it to a data frame because the object couldn't be found

as.data.frame(dat1UT)

#Issue here as data and time combine in the wrong format

dataGroupUT <- formatFields(
  dataGroup = dat1UT, 
  fieldID   = "id", 
  fieldDate = "DATE",
  fieldTime = "TIME",
  fieldLon  = "LON", 
  fieldLat  = "LAT")

#this fixes the issue
dataGroup$DateTime <- as.POSIXct(lubridate::hms(dat1UT$TIME) 
                                 + as.POSIXct(lubridate::dmy(dat1UT$DATE)))

str(dataGroupUT)

#use tripSplit to split up the data into discrete trips.


# here we know that the first points in the data set are from the colony center (in the windscoop)
colonyUT <- dataGroupUT %>% 
  summarise(
    Longitude = first(23.336389), 
    Latitude  = first(-71.963882)
  )

str(dataGroupUT)

#Changed the inner and outer buffs to 20 and duration to 4 to stop UT050 reading as multiple trips

tripsUT <- tripSplit(
  dataGroupUT,
  colonyUT,
  innerBuff  = 40,      # kilometers
  returnBuff = 40,
  duration   = 5,      # hours
  rmNonTrip  = FALSE
  #verbose = TRUE
)


mapTrips(trips = tripsUT, colony = colonyUT, colorBy = 'trip')

sumTripsUT <- tripSummary(trips = tripsUT, colony = colonyUT)

sumTripsUT

write.csv(sumTripsUT, 'Processed_Trips/Metrics/02_sumTripsUT.csv', row.names=F)

#manually removed non trips from above csv and added in missing duration

summary <- read.csv("Processed_Trips/Metrics/02_sumTripsUT_cleaned.csv")

head(summary)

# Format all time columns for trip splitting

UTDataForMetrics$timestamp <- as.POSIXct(lubridate::ymd(UTDataForMetrics$DATE) +
                                           lubridate::hms(UTDataForMetrics$TIME))

summary$departure = strptime(summary$departure, format = '%d/%m/%Y %H:%M', tz = 'GMT')

summary$return = strptime(summary$return, format = '%d/%m/%Y %H:%M', tz = 'GMT')

# Make a bank TripID column in the main data frame
UTDataForMetrics$TripID = 0.00

# Loop for BirdIDs
bird = unique(summary$ID)

for(i in 1:length(bird)){
  
  # Loop for TripID within bird
  trip = unique(summary$tripID[summary$ID==bird[i]])
  
  for(j in 1:length(trip)){
    
    UTDataForMetrics$TripID[
      UTDataForMetrics$BIRDID==bird[i] &
        UTDataForMetrics$timestamp >= summary$departure[summary$ID==bird[i] & summary$tripID==trip[j]] &
        UTDataForMetrics$timestamp <= summary$return[summary$ID==bird[i] & summary$tripID==trip[j]] ] = trip[j]
    
  }
}

#make a csv file that is just UT tracks including 0s for lat lons and trip ids
write.csv(UTDataForMetrics, 'Processed_Trips/02_Adding_Trip_IDs_UT.csv', row.names=F)


##########################
#####SV Tracks#####
##########################

SVDataForMetrics <- read.csv("Processed_Trips/01_POS_Extracted_SV.csv")



#Remove erroneous locations (where both Lat and lon are zero)
nrow(SVDataForMetrics)
SVDataForMetrics = SVDataForMetrics[SVDataForMetrics$ACC != 9999.999,]
nrow(SVDataForMetrics)

#check all observations are complete (code from amt package vignette)

all(complete.cases(SVDataForMetrics)) # no action required

#new datatime column formatted as a datetime
SVDataForMetrics$DateTime <- SVDataForMetrics$RTIME

#formatted as a timestamp
SVDataForMetrics <- SVDataForMetrics %>% mutate(DateTime = lubridate::dmy_hm(DateTime))


#parse date and time and create time stamps - note I changed date format from example

SVDataForMetrics$DATE <- lubridate::ymd(SVDataForMetrics$DateTime)

SVDataForMetrics$ts <- as.POSIXct(SVDataForMetrics$DATE +
                                    lubridate::hms(SVDataForMetrics$TIME))

lubridate::ymd(SVDataForMetrics$DATE)


#converting data from to tibble - this is from stack overflow not the cran I think
#dat1 <- as_tibble(DataForMetrics) %>% dplyr::select (LON = LON, LAT = LAT,
#t = ts, TIME = TIME, DATE = DATE, id=BIRDID)

dat1SV <- as_tibble(SVDataForMetrics) %>% dplyr::select (LON = LON, LAT = LAT,
                                                         TIME = TIME, DATE = DATE, id=BIRDID)

head(dat1SV)


#Now using the github example for track2iba

tibble(dat1SV)

#converted it to a data frame because the object couldn't be found

as.data.frame(dat1SV)

#Issue here as data and time combine in the wrong format

dataGroupSV <- formatFields(
  dataGroup = dat1SV, 
  fieldID   = "id", 
  fieldDate = "DATE",
  fieldTime = "TIME",
  fieldLon  = "LON", 
  fieldLat  = "LAT")

#this fixes the issue
#dataGroup$DateTime <- as.POSIXct(lubridate::hms(dat1$TIME) 
#+ as.POSIXct(lubridate::dmy(dat1$DATE)))

str(dataGroupSV)

#use tripSplit to split up the data into discrete trips.


# setting the new colony loc
colonySV <- dataGroupSV %>% 
  summarise(
    Longitude = first(5.159), 
    Latitude  = first(-71.890)
  )

str(dataGroupSV)


#setting inner and outer buff
tripsSV <- tripSplit(
  dataGroupSV,
  colonySV,
  innerBuff  = 40,      # kilometers
  returnBuff = 40,
  duration   = 5,      # hours
  rmNonTrip  = FALSE
  #verbose = TRUE
)


mapTrips(trips = tripsSV, colony = colonySV, colorBy = 'trip')

sumTripsSV <- tripSummary(trips = tripsSV, colony = colonySV)

sumTripsSV

write.csv(sumTripsSV, 'Processed_Trips/Metrics/02_sumTripsSV.csv', row.names=F)


###adding trip ID column

summarySV <- read.csv("Processed_Trips/Metrics/02_sumTripsSV_cleaned.csv")

head(summarySV)
SVDataForMetrics$timestamp <- as.POSIXct(lubridate::ymd(SVDataForMetrics$DATE) +
                                           lubridate::hms(SVDataForMetrics$TIME))

summarySV$departure = strptime(summarySV$departure, format = '%d/%m/%Y %H:%M', tz = 'GMT')

summarySV$return = strptime(summarySV$return, format = '%d/%m/%Y %H:%M', tz = 'GMT')

# Make a bank TripID column in the main data frame
SVDataForMetrics$TripID = 0.00

# Loop for BirdIDs
bird = unique(summarySV$ID)

for(i in 1:length(bird)){
  
  # Loop for TripID within bird
  trip = unique(summarySV$tripID[summarySV$ID==bird[i]])
  
  for(j in 1:length(trip)){
    
    SVDataForMetrics$TripID[
      SVDataForMetrics$BIRDID==bird[i] &
        SVDataForMetrics$timestamp >= summarySV$departure[summarySV$ID==bird[i] & summarySV$tripID==trip[j]] &
        SVDataForMetrics$timestamp <= summarySV$return[summarySV$ID==bird[i] & summarySV$tripID==trip[j]] ] = trip[j]
    
  }
}

write.csv(SVDataForMetrics, 'Processed_Trips/02_Adding_Trip_IDs_SV.csv', row.names=F)

################
#then combine both UT and SV csvs manually in excell
#read in the combined csv
test_df <- read.csv("Processed_Trips/02_Both_Col_Trip_IDs.csv")


#plot to check 
ggplot() +
  geom_point(aes(x = LON, y = LAT, colour = TripID), data = test_df)


##Removing zero lat long values
Removing_Zeros_Combined_tripID <- read.csv("Processed_Trips/02_Both_Col_Trip_IDs.csv")

nrow(Removing_Zeros_Combined_tripID)
Removing_Zeros_Combined_tripID = Removing_Zeros_Combined_tripID[Removing_Zeros_Combined_tripID$ACC != 9999.999,]
nrow(Removing_Zeros_Combined_tripID)

#plot to check - equator point is gone
ggplot() +
  geom_point(aes(x = LON, y = LAT, colour = TripID), data = Removing_Zeros_Combined_tripID)

###need to remove zero values from trip id column

library(dplyr)

test_remove <- Removing_Zeros_Combined_tripID
test_remove$TripID[test_remove$TripID == 0] <- NA    

nrow(test_remove)
test_remove2 <- test_remove[complete.cases(test_remove), ]            # Apply complete.cases function
nrow(test_remove2)

ggplot() +
  geom_point(aes(x = LON, y = LAT, colour = TripID), data = test_remove2)

##plot all together as a line plot
sel = test_remove2
bird = unique(sel$TripID)
col = rev(rainbow(length(bird)))
plot(sel$LON, sel$LAT, 'n')
for(i in 1:length(bird)) points(sel$LON[sel$TripID==bird[i]], sel$LAT[sel$TripID==bird[i]], col = col[i], 'l')

write.csv(test_remove2, 'Processed_Trips/02_All_Trips_Cleaned.csv', row.names=F)

#then take 02_All_Trips_Cleaned.csv into QGIS and trim off the overland sections

#END