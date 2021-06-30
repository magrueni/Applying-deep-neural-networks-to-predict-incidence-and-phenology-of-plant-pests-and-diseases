############################################################################
# Script for preparing the meteorological data  ############################
############################################################################
# Marc Grünig 01.06.2021 ###################################################
############################################################################


#libraries
library(raster)
library(dplyr)
library(tidyverse)
#define file path
fl_nme <- ""

# load meteodata
MeteoData <- read.table(paste(fl_nme,"/MeteoDaten/Data4Marc/Gridded_Data/Output_Daily/Gelfingen_DailyWeather.dat",sep=""),header=T)
# isolate the data for given time in 2019 (Mai to Octobre)
MeteoData2019 <- MeteoData[MeteoData$year == "2019",]
MeteoLongTerm <- MeteoData[MeteoData$year != "2019",]

# average the columns over all years
dat <- as.data.frame(matrix(ncol=ncol(MeteoData2019),nrow = nrow(MeteoData2019)))
colnames(dat) <- colnames(MeteoData)
dat[is.na(dat)] <- 0
years <- unique(MeteoLongTerm$year)

# average over all years
for(y in years){
  dat <- dat + MeteoData[MeteoData$year == y,]
}
dat <- dat/length(years)

# extract the Meteodata from 2019 for the needed timeperiod
MetDat <- MeteoData2019[121:273,]
plot(MetDat$DOY,MetDat$ET.PT)

# calculate degree days
gdd <- cumsum(MetDat$Ta)
plot(MetDat$DOY,gdd)

#calculate weekly averages
x <- c()
for(i in 1:53){x <- c(x,rep(i,7))}
x <- x[0:365]
MeteoData2019 <- cbind(MeteoData2019,x)
colnames(MeteoData2019) <- c(colnames(MeteoData),"week")
MetDat <- MeteoData2019[121:273,]
#Met.week <- MetDat %>% summarise_all("week")
Met.week <- aggregate(MetDat,by=list(MetDat$week),FUN=mean)
plot(Met.week$week,Met.week$Ta)
                      
## make some plots
plot(MeteoData2019$DOY,MeteoData2019$Ta,col="black",type="l",xlab="Day",ylab="mean temp")
lines(dat$DOY,dat$Ta,col="red")
legend("topright",c("1981-2018","2019"),fill=c("red","black"))
dev.print(png,paste(fl_nme,"/Graphs/Temperature.png",sep=""),height=800)

plot(MeteoData2019$DOY,MeteoData2019$Precip,col="black",type="l",xlab="Day",ylab="precipitation")
lines(dat$DOY,dat$Precip,col="red")
legend("topright",c("1981-2018","2019"),fill=c("red","black"))
dev.print(png,paste(fl_nme,"/Graphs/Precipitation.png",sep=""),height=800)

gdd.2019 <- cumsum(MeteoData2019$Ta)
gdd.long <- cumsum(dat$Ta)
plot(MeteoData2019$DOY,gdd.2019,col="black",type="l",xlab="Day",ylab="precipitation")
lines(dat$DOY,gdd.long,col="red")
legend("topleft",c("1981-2018","2019"),fill=c("red","black"))
dev.print(png,paste(fl_nme,"/Graphs/gdd.png",sep=""),height=800)


dat.long.season <- dat[121:273,]
dat.2019.season <- MeteoData2019[121:273,]

## make some plots
plot(dat.2019.season$DOY,dat.2019.season$Ta,col="black",type="l",xlab="Day",ylab="mean temp")
lines(dat.long.season$DOY,dat.long.season$Ta,col="red")
legend("topright",c("1981-2018","2019"),fill=c("red","black"))
dev.print(png,paste(fl_nme,"/Graphs/Temperature_season.png",sep=""),height=800)

plot(dat.2019.season$DOY,dat.2019.season$Precip,col="black",type="l",xlab="Day",ylab="precipitation")
lines(dat.long.season$DOY,dat.long.season$Precip,col="red")
legend("topright",c("1981-2018","2019"),fill=c("red","black"))
dev.print(png,paste(fl_nme,"/Graphs/Precipitation_season.png",sep=""),height=800)

gdd.2019.season <- cumsum(dat.2019.season$Ta)
gdd.long.season <- cumsum(dat.long.season$Ta)
plot(dat.2019.season$DOY,gdd.2019.season,col="black",type="l",xlab="Day",ylab="precipitation")
lines(dat.long.season$DOY,gdd.long.season,col="red")
legend("topleft",c("1981-2018","2019"),fill=c("red","black"))
dev.print(png,paste(fl_nme,"/Graphs/gdd_season.png",sep=""),height=800)
