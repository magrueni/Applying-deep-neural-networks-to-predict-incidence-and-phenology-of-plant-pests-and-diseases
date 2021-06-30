############################################################################
# Script for building phenology models  ####################################
############################################################################
# Marc Grünig 01.06.2021 ###################################################
############################################################################

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


#############
#### plotting

locations <- c("Gel","Kle")
labfield <- c("Lab","Field")

# calculate percentages
r <- 0
for(l in locations){
  for(x in labfield){
    r <- r+1
    
    # load data
    cat_prc <- read.table(paste(fl_nme,"/Results/",x,"pics_probabilities_all/prct_",l,"_all.csv",sep=""),sep=";",header=T)
    
    if(x == "Field"){cat_prc <- cat_prc[2:20,]}
    assign(paste("fm_prob_loc",r,sep=""),cat_prc[,3])
  }
}

# combine data of different locations and field and lab pics
fm_prob_all_loc <- cbind(fm_prob_loc1,fm_prob_loc2,fm_prob_loc3,fm_prob_loc4)

# do it in a loop or specify with i <- 1
#for(i in 1:4){
i <- 4
cat_prc <- as.data.frame(cbind(cat_prc[1:19,1],fm_prob_all_loc[,i],c(1:nrow(fm_prob_all_loc))))
colnames(cat_prc) <- c("datum","fm_prob","id")

# add weeks before we started sampling
Emptydat.before <- as.data.frame(cbind(as.numeric(seq(1,c(35-nrow(cat_prc)),1)),as.numeric(rep(0,c(35-nrow(cat_prc)))),as.numeric(rep(0,c(35-nrow(cat_prc))))))
Emptydat.before <- apply(Emptydat.before, 2, function(x) as.numeric(as.character(x)))

# combine the recorded data with the empty dataframe from before
colnames(Emptydat.before) <- colnames(cat_prc)
dat.new <- rbind(Emptydat.before,cat_prc)

# then combine with meteo data
Dat <- cbind(dat.new[1:nrow(dat.new),],Met.week[1:35,])
Dat <- Dat[1:35,]

###########
# building the models
###########

# plot the data to get impression
plot(Dat[,"week"],Dat[,"fm_prob"],type="l", main="FM in treated site Gelfingen",xlab="week", ylab="% leaf damaged")

# build GLM with all predictors
step.glm <- glm(Dat[,"fm_prob"]/100 ~ poly(Ta,2) + poly(Precip,2) + poly(gdd5,2) + poly(SRad,2) + poly(diur,2),data = Dat,family = "binomial")
summary(step.glm)

# check D2 value
ecospat.adj.D2.glm(step.glm)

# annova
anova.table <- anova(step.glm, test="Chisq")
anova.table

#### Now single predictor models
# first DOY
mod.doy <- glm(Dat[,"fm_prob"]/100 ~ poly(DOY),data = Dat,family = "binomial")
#summary(mod.doy)
ecospat.adj.D2.glm(mod.doy)

# GDD
mod.gdd <- glm(Dat[,"fm_prob"]/100 ~ poly(gdd5,2),data = Dat,family = "binomial")
#summary(mod.gdd)
ecospat.adj.D2.glm(mod.gdd)

# mean temp
mod.Ta <- glm(Dat[,"fm_prob"]/100 ~ poly(Ta,2),data = Dat,family = "binomial")
#summary(mod.Ta)
ecospat.adj.D2.glm(mod.Ta)

# precipitation
mod.prec <- glm(Dat[,"fm_prob"]/100 ~ poly(Precip,2),data = Dat,family = "binomial")
#summary(mod.prec)
ecospat.adj.D2.glm(mod.prec)

# solar radiation
mod.srad <- glm(Dat[,"fm_prob"]/100 ~ poly(SRad,2),data = Dat,family = "binomial")
#summary(mod.srad)
ecospat.adj.D2.glm(mod.srad)

# diurnal temperature range
mod.diur <- glm(Dat[,"fm_prob"]/100 ~ poly(diur,2),data = Dat,family = "binomial")
#summary(mod.diur)
ecospat.adj.D2.glm(mod.diur)


# plot to check
pred.val <- predict(mod.gdd,newdata=data.frame(Met.week),type="response")
par(xpd=F)
plot(Dat[,"week"],Dat[,"fm_prob"]/100,type="l",xlab="week after January 1, 2019", ylab="% of leaves damaged / 100",bty="n",ylim=c(0,1),xlim=c(0,32),cex.axis=1.4,cex.lab=1.4,lwd=2)
lines(1:length(as.numeric(pred.val)),as.numeric(pred.val),col="red",lwd=2)
legend("topleft",c("DNN reconstructed phenology","phenology model based on degree-days"),col=c("black","red"),lwd=2, lty=c(1,1),bty="n",cex=1.4)
abline(v=15, col="grey",cex=0.5,lty=4)
abline(v=23.5, col="grey",cex=0.5,lty=4)
#abline(v=28.1, col="grey",cex=0.5,lty=4)
par(xpd=TRUE)
#text(x=2.1,y=-20,"May 1",font=3,cex=1)
text(x=15,y=0.8,"May 1",font=3,cex=1.2)
text(x=23.5,y=0.8,"July 1",font=3,cex=1.2)
#text(x=28.1,y=50,"August 1",font=3,cex=1)


### do the final plot
letters <- c("a)","b)","c)","d)")
par(mfrow=c(2,2),mar=c(4,4,1,2),xpd=F,oma=c(2,0.5,0,0))
# now do the models for all locations and treatments
for(i in 1:4){
  #i <- 2
  cat_prc <- as.data.frame(cbind(expl[1:19,1],fm_prob_all_loc[,i],c(1:nrow(fm_prob_all_loc))))
  colnames(cat_prc) <- c("datum","fm_prob","id")
  
  # add weeks before
  Emptydat.before <- as.data.frame(cbind(as.numeric(seq(1,c(35-nrow(cat_prc)),1)),as.numeric(rep(0,c(35-nrow(cat_prc)))),as.numeric(rep(0,c(35-nrow(cat_prc))))))
  Emptydat.before <- apply(Emptydat.before, 2, function(x) as.numeric(as.character(x)))
  #Emptydat.before <- cbind(Emptydat.before,Emptydat.before[,c(3,3,3)])
  
  colnames(Emptydat.before) <- colnames(cat_prc)
  dat.new <- rbind(Emptydat.before,cat_prc)
  
  # combine with meteo data
  Dat <- cbind(dat.new[1:nrow(dat.new),],Met.week[1:35,])
  Dat <- Dat[1:35,]
  #colnames(Dat)[8] <- "JulianDay"
  # model
  plot(Dat[,"week"],Dat[,"fm_prob"]/100,type="l",xlab="", ylab="",ylim=c(0,0.9))
  #new.val <- seq(1,nrow(Dat),1)
  scale(Dat[,"fm_prob"])
  
  step.glm <- glm(Dat[,"fm_prob"]/100 ~ poly(Ta,2) + poly(Precip,2) + poly(gdd5,2) + poly(SRad,2) + poly(diur,2),data = Dat,family = "binomial")
  summary(step.glm)
  
  ecospat.adj.D2.glm(step.glm)
  
  anova.table <- anova(step.glm, test="Chisq")
  anova.table
  
  mod.doy <- glm(Dat[,"fm_prob"]/100 ~ poly(DOY,2),data = Dat,family = "binomial")
  #summary(mod.doy)
  ecospat.adj.D2.glm(mod.doy)
  
  
  mod.gdd <- glm(Dat[,"fm_prob"]/100 ~ poly(gdd5,2),data = Dat,family = "binomial")
  #summary(mod.gdd)
  ecospat.adj.D2.glm(mod.gdd)
  
  mod.Ta <- glm(Dat[,"fm_prob"]/100 ~ poly(Ta,2),data = Dat,family = "binomial")
  #summary(mod.Ta)
  ecospat.adj.D2.glm(mod.Ta)
  
  
  mod.prec <- glm(Dat[,"fm_prob"]/100 ~ poly(Precip,2),data = Dat,family = "binomial")
  #summary(mod.prec)
  ecospat.adj.D2.glm(mod.prec)
  
  mod.srad <- glm(Dat[,"fm_prob"]/100 ~ poly(SRad,2),data = Dat,family = "binomial")
  #summary(mod.srad)
  ecospat.adj.D2.glm(mod.srad)
  
  mod.diur <- glm(Dat[,"fm_prob"]/100 ~ poly(diur,2),data = Dat,family = "binomial")
  #summary(mod.diur)
  ecospat.adj.D2.glm(mod.diur)
  
  #write.table(anova.table,paste(fl_nme,"/Results/PhenologicalModels/FM_Gelfingen_26032020_treated.csv",sep=""),sep=";")
  pred.val <- predict(mod.gdd,newdata=data.frame(Met.week),type="response")
  pearson.cor <- cor(as.numeric(pred.val)[1:35],Dat[,"fm_prob"]/100,method="pearson")
  cat("Correlation of", pearson.cor)
  par(xpd=F)
  #plot(Dat[,"week"],Dat[,"fm_prob"]/100,type="l",xlab="week after January 1, 2019", ylab="% of leaves damaged / 100",bty="n",ylim=c(0,1),xlim=c(0,32),cex.axis=1.4,cex.lab=1.4,lwd=2)
  lines(1:length(as.numeric(pred.val)),as.numeric(pred.val),col="red",lwd=2)
  #legend("topleft",c("DNN reconstructed phenology","phenology model based on degree-days"),col=c("black","red"),lwd=2, lty=c(1,1),bty="n",cex=1.4)
  abline(v=15, col="grey",cex=0.5,lty=4)
  abline(v=23.5, col="grey",cex=0.5,lty=4)
  #abline(v=28.1, col="grey",cex=0.5,lty=4)
  par(xpd=TRUE)
  #text(x=2.1,y=-20,"May 1",font=3,cex=1)
  if(i %in% c(1,2)){
    text(x=15,y=-0.25,"May 1",font=3,cex=1.2)
    text(x=23.5,y=-0.25,"July 1",font=3,cex=1.2)
  }
  if(i %in% c(3,4)){
    mtext(side = 1,"week",line = 3,cex=1.2)
  }
  if(i %in% c(1,3)){
    mtext(side = 2,"% leaf damaged",line = 3,cex=1.2)
  }
  #text(x=28.1,y=50,"August 1",font=3,cex=1)
  text(x=2,y=0.80,letters[i],cex=1.2)
} 

dev.print(png,paste(fl_nme,"/Graphs/Fig5_4panels2.png",sep=""),height=500)

#plot legend
plot.new()
legend("topleft",c("DNN reconstructed phenology","Degree-day model"),col=c("black","red"),lwd=2, lty=c(1,1),bty="n",cex=1.4)
dev.print(png,paste(fl_nme,"/Graphs/Fig5_legend.png",sep=""),height=500)

