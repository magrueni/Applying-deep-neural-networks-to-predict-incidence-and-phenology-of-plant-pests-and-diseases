############################################################################
# Script for calculating the phenology of all damage classes  ##############
############################################################################
# Marc Grünig 01.06.2021 ###################################################
# cleaned ++++++++++++++ ###################################################
############################################################################

#libraries
library(raster)
library(dplyr)
library(tidyverse)
library(ecospat)
#define file path
fl_nme <- "~/mnt/Data-Work-CH/22_Plant_Production-CH/226.12 Entomologie Obstbau/2018-2020_PhD_NCCS/GRMR/R_Virtualmachine/Chapter3"

# define categoires
Categories <- c("Healthy","FM","PhysioDam","BrownSpots","Lepidoptera","Mehltau")
locations <- c("Gel","Kle")
labfield <- c("Lab","Field")
letters <- c("a","b","c","d")

# prepare plot
par(mfrow=c(2,2),mar=c(2,2,2,4),oma=c(3,3,3,3),xpd=F)
r <- 0
for(l in locations){
  for(x in labfield){
    par(xpd=F)
  r <- r+1
  cat_prc <- read.table(paste(fl_nme,"/Results/",x,"pics_29092020_probabilities_all/prct_",l,"_all.csv",sep=""),sep=";",header=T)
  if(x == "Field"){
    cat_prc <- cat_prc[2:20,]
  }
   
  plot(1:19,cat_prc$healthy_prob,col="darkgreen",type="l",lwd=2,lty=1,bty="n",xlim=c(0,20),ylim=c(0,100))
  lines(1:19,cat_prc$fm_prob,col="red",type="l",lwd=2,lty=1)
  lines(1:19,cat_prc$pyhsiodam_prob,col="blue",type="l",lwd=2,lty=1)
  lines(1:19,cat_prc$lepi_prob,col="darkgrey",type="l",lwd=2,lty=1)
  lines(1:19,cat_prc$mehltau_prob,col="lightgrey",type="l",lwd=2,lty=1)
  lines(1:19,cat_prc$brownspots_prob,col="brown",type="l",lwd=2,lty=1)
  
  
  abline(v=6.5, col="grey",cex=0.5,lty=4)
  abline(v=15.1, col="grey",cex=0.5,lty=4)
  par(xpd=TRUE)
  text(x=6.5,y=95,"June 1",font=3,cex=1)
  text(x=15.1,y=95,"August 1",font=3,cex=1)
  
  if(r==1|r==3){mtext(side=2, "adults in trap",line=3)}
  if(r==3|r==4){mtext(side=1,"week after April 15, 2019",line=3)}
  
  
  text(x=2,y=95,letters[r],cex=1.4)
  
  }
}

dev.print(png,paste(fl_nme,"/Graphs/Phenologycurve_All_new.png",sep=""),height=800)
plot.new()
legend("right",c("Undamaged","PLBM","Physical","Lepidoptera","Mildew","Brown spots"),lty=c(1,1,1,1,1),col=c("darkgreen","red","blue","darkgrey","lightgrey","brown"),cex = 1,bty="n")
dev.print(png,paste(fl_nme,"/Graphs/Phenologycurve_Legend.png",sep=""),height=800)
