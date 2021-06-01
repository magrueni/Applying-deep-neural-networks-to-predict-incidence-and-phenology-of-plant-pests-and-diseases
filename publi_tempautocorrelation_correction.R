############################################################################
# Script for phenology models correcting for temporal autocorrelation  #####
############################################################################
# Marc Grünig 01.06.2021 ###################################################
# cleaned ++++++++++++++ ###################################################
############################################################################

### I used GLARMA for correcting for temporal autocorrelation

#libraries
library(raster)
library(dplyr)
library(tidyverse)
library(ecospat)

#define file path
fl_nme <- ""

# load meteo data
MeteoData <- read.table(paste(fl_nme,"/MeteoDaten/Data4Marc/Gridded_Data/Output_Daily/Kleinwangen_DailyWeather.dat",sep=""),header=T)
# isolate the data for given time in 2019 (Mai to Octobre)
MeteoData2019 <- MeteoData[MeteoData$year == "2019",]
MetDat <- MeteoData2019[121:273,]

#calculate diurnal range
diur <- MeteoData2019$Tmax - MeteoData2019$Tmin
MeteoData2019 <- cbind(MeteoData2019,diur)

# calculate degree days
gdd <- MeteoData2019$Ta
MeteoData2019 <- cbind(MeteoData2019,gdd)
MeteoData2019[MeteoData2019$gdd < 5,]$gdd <- 0
gdd5 <- cumsum(MeteoData2019$gdd)
#plot(MeteoData2019$DOY,gdd)

#calculate weekly averages
x <- c()
for(i in 1:53){x <- c(x,rep(i,7))}
x <- x[0:365]
MeteoData2019 <- cbind(MeteoData2019,gdd5,x)
colnames(MeteoData2019) <- c(colnames(MeteoData),"diur","gdd","gdd5","week")

#agregate for the week with mean
MetDat <- MeteoData2019[,5:ncol(MeteoData2019)]
Met.week <- aggregate(MetDat,by=list(MetDat$week),FUN=mean)

#agregate the datum with maximum
datwek <- MeteoData2019[,1:4]
Dat.week <- aggregate(datwek,by=list(MetDat$week),FUN=max)

#combine again days and meteodat
Met.week <- cbind(Dat.week, Met.week)

# define locations and recording conditions
locations <- c("Gel","Kle")
labfield <- c("Lab","Field")
c <- "FM"

# calculate percentages
r <- 0
for(l in locations){
  for(x in labfield){
    r <- r+1
    cat_prc <- read.table(paste(fl_nme,"/Results/",x,"pics_29092020_probabilities_all/prct_",l,"_all.csv",sep=""),sep=";",header=T)
    ## plotting
    if(x == "Field"){cat_prc <- cat_prc[2:20,]}  #plot(cat_prc[,"id"],cat_prc[,3],type="l") 
    assign(paste("fm_prob_loc",r,sep=""),cat_prc[,3])
  }
}

fm_prob_all_loc <- cbind(fm_prob_loc1,fm_prob_loc2,fm_prob_loc3,fm_prob_loc4)#[3:18]


# now do the models for all locations and treatments
# set the number for treat and loc
i <- 4
cat_prc <- as.data.frame(cbind(cat_prc[1:19,1],fm_prob_all_loc[,i],c(1:nrow(fm_prob_all_loc))))
colnames(cat_prc) <- c("datum","fm_prob","id")

# add weeks before sampling
Emptydat.before <- as.data.frame(cbind(as.numeric(seq(1,c(35-nrow(cat_prc)),1)),as.numeric(rep(0,c(35-nrow(cat_prc)))),as.numeric(rep(0,c(35-nrow(cat_prc))))))
Emptydat.before <- apply(Emptydat.before, 2, function(x) as.numeric(as.character(x)))
colnames(Emptydat.before) <- colnames(cat_prc)

#combine with sampling data
dat.new <- rbind(Emptydat.before,cat_prc)

# combine with meteo data
Dat <- cbind(dat.new[1:nrow(dat.new),],Met.week[1:35,])
Dat <- Dat[1:35,]

# model
dev.off()
# visualize data row
plot(Dat[,"week"],Dat[,"fm_prob"],type="l", main="FM in treated site Gelfingen",xlab="week", ylab="% leaf damaged")


#### check for temporal autocorrelation ####
# here we remove the temporal autocorrelation by subtracting the previous value in the data row

dat.new <- round(Dat[,"fm_prob"]/100*400,0)
new.data.new <- c()
for(x in 1:(length(dat.new)-1)){
  v1 <- dat.new[x]
  v2 <- dat.new[x+1]
  new.v <- v2-v1
  if(new.v < 0){new.v <- new.v*-1}
  new.data.new <- c(new.data.new,new.v)
}
new.dat.new <- c(na.omit(new.data.new),0)

# now we use the glarma function to get the AIC
# first full model
regressors <- cbind(poly(Dat$Ta,2),poly(Dat$Precip,2),poly(Dat$gdd5,2),poly(Dat$SRad,2),poly(Dat$diur,2))
colnames(regressors) <- c("Ta","Ta2","Prec","Prec2","gdd","gdd2","rad","rad2","diur","diur2")
new.mod <- glarma(new.dat.new,regressors)
summary.glarma(new.mod)

# then single predictor models
glm.gdd <- glm(Dat[,"fm_prob"]/100 ~ poly(gdd5,2),data = Dat,family = "binomial")
summary(glm.gdd)

mod.gdd <- glarma(dat.new,regressors[,c(5,6)])
summary(mod.gdd)

mod.Ta <- glarma(dat.new,regressors[,c(1,2)])
summary(mod.Ta)


mod.prec <- glarma(new.dat.new,regressors[,c(3,4)])
summary(mod.prec)
#ecospat.adj.D2.glm(mod.prec)

mod.srad <- glarma(new.dat.new,regressors[,c(7,8)])
summary(mod.srad)
#ecospat.adj.D2.glm(mod.srad)

mod.diur <- glarma(new.dat.new,regressors[,c(9,10)])
summary(mod.diur)
#ecospat.adj.D2.glm(mod.diur)

#write.table(anova.table,paste(fl_nme,"/Results/PhenologicalModels/FM_Gelfingen_26032020_treated.csv",sep=""),sep=";")
pred.val <- predict(mod.gdd,newdata=data.frame(Met.week),type="response")
par(xpd=F)
plot(Dat[,"week"],Dat[,"fm_prob"]/100,type="l",xlab="week after January 1, 2019", ylab="% of leaves damaged",bty="n",ylim=c(0,1),xlim=c(0,32))
lines(1:length(as.numeric(pred.val)),as.numeric(pred.val),col="red")
legend("topleft",c("phenology obtained from DNN","modelled phenology"),col=c("black","red"), lty=c(1,1),bty="n")
abline(v=15, col="grey",cex=0.5,lty=4)
abline(v=23.5, col="grey",cex=0.5,lty=4)
#abline(v=28.1, col="grey",cex=0.5,lty=4)
par(xpd=TRUE)
#text(x=2.1,y=-20,"May 1",font=3,cex=1)
text(x=15,y=50,"May 1",font=3,cex=1)
text(x=23.5,y=50,"July 1",font=3,cex=1)
#text(x=28.1,y=50,"August 1",font=3,cex=1)

dev.print(png,paste(fl_nme,"/Graphs/FM_Kleinwangen_14052020_gdd_lab.png",sep=""),height=500)



