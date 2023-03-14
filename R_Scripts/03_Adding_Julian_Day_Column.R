######################################
### Adding a column for Julian Day ###
###           26_01_2023           ###
######################################

#Before this all overland sections were trimmed in QGIS

rm(list=ls())
df <- read.csv("Processed_Trips/03_Trimmed_Trips.csv")

plot(df$LON, df$LAT)
df <- df %>% mutate(DateTime = lubridate::dmy_hm(DateTime))

library(tidyverse)
library(lubridate)

my_date <- as.Date("2022-01-31")          # Create example date
my_date

my_julian2 <- format(my_date, "%j")       # Convert date to Julian day
my_julian2                                # Print Julian day

#format as Julian
df$DateforJ <- df$DateTime

df$Julian <- format(df$DateforJ, "%j")

write.csv(df,"Processed_Trips/03_Trimmed_Trips.csv")


#END


#extra stuff from the old script, need environemntal covs 

#plot
plot(df$Julian,df$dist_ice_edge_15)

plot(df2$dist_ice_edge_15,df2$Julian)


#sep by behaviour
df1 <- df %>% filter(df$state == 1)
df2 <- df %>% filter(df$state == 2)
df3 <- df %>% filter(df$state == 3)

#plot

p <- ggplot(data=df2, aes(x=Julian, y=dist_ice_edge_15, colour=Colony)) +
  geom_point()

p


ice_edge_plot <- ggplot(df2, aes(x = Julian, y = dist_ice_edge_15, fill = Colony)) +
  # my_scalebox +
  theme_classic()+
  geom_boxplot()+
  labs(
    title = "Distance to ice edge",
    x = "Distance (km)",
    y = "Colony")

