########################################
### 07_Compare_SIC_to_Sea_Ice_Charts ###
###         07/03/2023               ###
########################################

## January 21st 2022

birdSV = unique(comp$BIRDID)

# Sea ice chart concentration (observed
for(i in 1:6){
  #for(i in 2:length(bird)){
  #this makes the data - using the range of the minumum and maximum sst for the entire tracked period
  tmpSV=density(comp$CT[comp$BIRDID == birdSV[i]], bw = 8, na.rm=T, from=min(comp$CT,na.rm=T), to=max(comp$CT,na.rm=T))
  #x here is the range of SST (regularised to intervals)
  #note - str = structure function
  print(tmpSV$bw)
  #saves the x value
  CTmy.xSV=tmpSV$x
  #pulls out the y value and standardises it to sum to 1- y is the density for individuals in the loop
  tmpSV = tmpSV$y/sum(tmpSV$y)
  #for the first itteration -renaming standardised y values then on subsequent loops - this makes a matrix of a column for each individual
  if(i==1){tmpSV2 = tmpSV}else{tmpSV2=cbind(tmpSV2,tmpSV)}
}


#takes the matrix of a column for each bird - across the rows - take the mean (1 = apply across rows)
CTtmpSV2=apply(tmpSV2,1,mean)
plot(CTmy.xSV,CTtmpSV2, 'l')
points(CTmy.xSV,tmpSV2[,1],'l',col='grey')

# Example plot - creates a blank plot with full range - n is base r for blank plot
plot(range(CTmy.xSV),range(tmpSV2),'n', xlab = "AMSR2 vs Charts on Jan 21 (SV)", ylab = "Density")
for(i in 1:ncol(tmpSV2)) points(CTmy.xSV,tmpSV2[,i],'l',col='grey')
#points(CTmy.xSV,CTtmpSV2, 'l', col = "red")
points(CTmy.xSV,CTtmpSV2, type = 'l', lwd = 2,  lty = 1, col = "red")

# Passive Microwave Concentration (AMSR2)
for(i in 1:6){
  #for(i in 2:length(bird)){
  #this makes the data - using the range of the minumum and maximum sst for the entire tracked period
  sicSV=density(comp$concentration[comp$BIRDID == birdSV[i]], bw = 10, na.rm=T, from=min(comp$concentration,na.rm=T), to=max(comp$concentration,na.rm=T))
  #x here is the range of SST (regularised to intervals)
  #note - str = structure function
  print(sicSV$bw)
  #saves the x value
  concmy.xSV=sicSV$x
  #pulls out the y value and standardises it to sum to 1- y is the density for individuals in the loop
  sicSV = sicSV$y/sum(sicSV$y)
  #for the first itteration -renaming standardised y values then on subsequent loops - this makes a matrix of a column for each individual
  if(i==1){sicSV2 = sicSV}else{sicSV2=cbind(sicSV2,sicSV)}
}


#takes the matrix of a column for each bird - across the rows - take the mean (1 = apply across rows)
conctmpSV2=apply(sicSV2,1,mean)
plot(concmy.xSV,conctmpSV2, 'l')
points(concmy.xSV,sicSV2[,1],'l',col='grey')

# Example plot - creates a blank plot with full range - n is base r for blank plot
plot(range(concmy.xSV),range(sicSV2),'n', xlab = "AMSR2 vs Charts on Jan 21 (SV)", ylab = "Density")
for(i in 1:ncol(sicSV2)) points(concmy.xSV,sicSV2[,i],'l',col='grey')
points(concmy.xSV,conctmpSV2, 'l', col = "darkcyan")
points(concmy.xSV,conctmpSV2, type = 'l', lwd = 2,  lty = 1, col = "darkcyan")

# Plot both

### combine
#Combining all on one plot - works 
#CT range is bigger so use that to set extent of plot
plot(range(CTmy.xSV),range(tmpSV2),'n', xlab = "AMSR2 vs Sea Ice Chart concentration for SV birds on 21/01/22", ylab = "Density")
for(i in 1:ncol(sicSV2)) points(concmy.xSV,sicSV2[,i],'l',col='cadetblue')
for(i in 1:ncol(tmpSV2)) points(CTmy.xSV,tmpSV2[,i],'l',col='coral')
points(CTmy.xSV,CTtmpSV2, 'l', col = "red", lwd = 2)
points(concmy.xSV,conctmpSV2, 'l', col = "darkcyan", lwd = 2)
legend(25, 0.0085, legend=c("Passive Microwave (AMSR2)", "Sea Ice Chart"),
       col=c("cadetblue", "red"), lty=1, cex=0.8, lwd = 2)



#end 


