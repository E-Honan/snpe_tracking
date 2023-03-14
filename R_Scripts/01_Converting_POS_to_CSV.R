############################################
### Converting .POS files to .csv format ###
###              26_01_2023              ###
############################################

### Combine .pos data from Pathtrack loggers reprocessed for altitude correction at both sites
### This is all Ewan's code bar me adding the lubridate section

rm(list=ls())
install.packages("readxl")
install.packages("tidyverse")
install.packages("lubridate")
library(lubridate)
library(readxl)
library(tidyverse)

#Path to directories containing POS file
path.in = ("Raw_Tracks/POS_Files/")
list.files(path.in)

#Metadata
meta = 
  data.frame(read.csv("Meta_Data/Logger_Details/Logger_Dep_Details.csv"))

meta$BIRDID = as.character(meta$BIRDID)


meta$deploy_rtime <- as.POSIXct(lubridate::dmy(meta$DEP_DATE) +
                                  
                                  lubridate::hm(meta$REL_TIME))


#meta$recap_rtime = strptime(paste(meta$RECAP_DATE, substr(meta$RECAP_TIME,12,19)), '%Y-%m-%d %H:%M:%S', tz='GMT')

head(meta)

# Loop for files and directories.

for(i in 1:length(path.in)){
  #i=1
  my.files = list.files(path.in[i], pattern = 'pos')
  my.files = my.files[my.files!='Obs100122_155109_Tag43664.pos']
  
  for(j in 1:length(my.files)){	
    #j=1
    tmp = read.table(paste(path.in[i], my.files[j], sep='/'), skip=5, sep=',')
    names(tmp) = c('DAY','MONTH','YEAR','HOUR','MINUTE','SECOND','SOD','N_SAT','LAT','LON','ALT','CLOCK_OFFSET','ACC','BAT','V1','V2')
    tmp$RTIME = strptime(paste(tmp$DAY,'/',tmp$MONTH,'/',tmp$YEAR,' ',tmp$HOUR,':',tmp$MINUTE,':',tmp$SECOND, sep=''), '%d/%m/%y %H:%M:%S', tz ='GMT')
    
    # Relevant row of deployment table
    z = which(meta$GPS_SN==substr(my.files[j],21,25))
    
    # Add bird ID and select other relevant columns
    tmp = data.frame(BIRDID = meta$BIRDID[z], tmp[,c('RTIME','LON','LAT','ALT','ACC','BAT'),])
    
    # Crop to times logger was on bird
    #		dim(tmp)
    #		tmp = tmp[as.numeric(tmp$RTIME)>=as.numeric(meta$deploy_rtime[z]),]
    #		dim(tmp)
    #		if(is.na(meta$recap_rtime[z])==F) tmp = tmp[as.numeric(tmp$RTIME)>=as.numeric(meta$deploy_rtime[z]),]
    #		dim(tmp)
    
    # Add to other data
    
    if(i==1 & j==1){
      
      dat = tmp
      
    }else{
      
      dat = rbind(dat, tmp)
      
    }
    
  }
}

# Sort by BIRDID and TIME
dat = dat[order(as.character(dat$BIRDID), as.numeric(dat$RTIME)),]
dim(dat)
summary(dat)

write.csv(summary(dat),'Processed_Trips/Metrics/01_summary_POS_Extracted_all.csv')

# Write to file
write.csv(dat, 'Processed_Trips/01_POS_Extracted_all.csv', row.names=F)

# Plot by bird to check
sel = dat[dat$ACC!=9999.999,]
bird = unique(sel$BIRDID)
col = rev(rainbow(length(bird)))
plot(sel$LON, sel$LAT, 'n')
for(i in 1:length(bird)) points(sel$LON[sel$BIRDID==bird[i]], sel$LAT[sel$BIRDID==bird[i]], col = col[i], 'l')


# End
