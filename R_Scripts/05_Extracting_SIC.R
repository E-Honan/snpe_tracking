###############################################
### Extracting  AMSR2 sea ice concentration ###
###             27/01/2023                  ###   
###############################################


####ALSO FIRST VISUALISATIONS including naming trips on the plot

library(tidyverse)
library(lubridate)

source("R_Scripts/R_Functions/05_Extract_AMSR2_Function.R")

#path to sea ice concentration geotiffs
path <- 'Environmental_Data/Sea_Ice/AMSR2/'

#csv of trips with overland portions trimmed off

df <- read.csv('Trip_Covariates/04_Trips_with_Depth_Slope.csv')

#plot to check 
trimmed_trips_plot <- ggplot() +
  geom_point(aes(x = LON, y = LAT, colour = TripID), data = df)

trimmed_trips_plot

#identify trips by number and ID on the plot
trimmed_trips_plot + geom_text(aes(x = LON, y = LAT, label=TripID),check_overlap = TRUE, data = df)

#why do I need to run this section? Doesn't work without it but not sure why?

df <- df %>% as_tibble()
df <- df %>% mutate(datetime = lubridate::dmy_hm(RTIME))

nrow(df)
df = df[df$ACC != 9999.999,]
nrow(df)

require(raster)

##extract sic at points
SIC <- extract_AMSR2(df, path)
SIC$concentration[SIC$concentration > 100] <- NA

#Plotting concentration
head(SIC)

hist(SIC$concentration)


p <- ggplot(SIC, aes(x= concentration)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="pink")+
  theme_classic()

plot(p)

#write to csv
write.csv(SIC, "Trip_Covariates/05_Trips_with_SIC.csv")


#end

###############################################################

#old plots from when I recorded it all as max 12%...

##compare 12%  locs to all 
ggplot() + 
  geom_point(aes(x = LON, y = LAT), data = No_Land_SIC_UT_SV) +
  geom_point(aes(x = LON, y = LAT), data = twelvepercentconc, colour = "red")+
  theme_classic()

##compare 0% locs to all 
ggplot() + 
  geom_point(aes(x = LON, y = LAT), data = No_Land_SIC_UT_SV) +
  geom_point(aes(x = LON, y = LAT), data = zeropercentconc, colour = "red")+
  theme_classic()

#COMPARE 0%, 12% AND ALL

ggplot() + 
  geom_point(aes(x = LON, y = LAT), data = No_Land_SIC_UT_SV) +
  geom_point(aes(x = LON, y = LAT), data = zeropercentconc, colour = "red")+
  theme_classic()+
  geom_point(aes(x = LON, y = LAT), data = twelvepercentconc, colour = "darkviolet")

##export a csv with a concentration column 

view(No_Land_SIC_UT_SV)

write.csv(No_Land_SIC_UT_SV,"C:/Users/emhon/OneDrive/Documents/R_Projects/SNPE_Tracking_2022/Analyses/SIC/Extract_AMSR2_UT_SV_trimmed_19_10.csv", row.names = FALSE)

write.csv(twelvepercentconc,"C:/Users/emhon/OneDrive/Documents/R_Projects/SNPE_Tracking_2022/Analyses/SIC/12percentsicpoints.csv", row.names = FALSE)


#end
