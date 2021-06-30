############################################################################
# Script to plot PLBM phenology ############################################
############################################################################
# Marc Grünig 01.06.2021 ###################################################
############################################################################

# path
fl_nme <- ""

# load the data from the extension group with the trapping data
fallen.tab <- read.csv(paste(fl_nme, "FallenFaenge_19.csv",sep=""),sep=";",header=T)
head(fallen.tab)

# extract the data for PLBM
tab.fm <- fallen.tab[fallen.tab$Ravageur == "Fleckenminiermotten",]
head(tab.fm)

# extract the data for our locations
tab.gelfingen <- tab.fm[tab.fm$Commune == "Hitzkirch",]
tab.kleinwangen <- tab.fm[tab.fm$Commune == "Hohenrain",]

# extract the data for 2019 and put into useful format
tab.gelfingen <- tab.gelfingen[grepl("2019",tab.gelfingen$Date),]
tab.gelfingen.fin <- as.data.frame(cbind(as.numeric(c(nrow(tab.gelfingen):1)),as.vector(tab.gelfingen$Date),as.numeric(as.vector(tab.gelfingen$Nombre.d.individus))))
tab.gelfingen.fin[,1] <- as.numeric(as.character(tab.gelfingen.fin[,1]))
tab.gelfingen.fin[,3] <- as.numeric(as.character(tab.gelfingen.fin[,3]))
head(tab.gelfingen.fin)
colnames(tab.gelfingen.fin) <- c("ID","date","count")
tab.gelfingen.fin <- tab.gelfingen.fin[order(tab.gelfingen.fin$ID),]
tab.gelfingen.fin <- cbind(tab.gelfingen.fin[5:21,2:3],c(1:17))
colnames(tab.gelfingen.fin) <- c("date","count","ID")
head(tab.gelfingen.fin)
empty <- c(rep(NA,ncol(tab.gelfingen.fin)))
tab.gelfingen.fin <- rbind(empty,empty,tab.gelfingen.fin)

# plot to get an idea
plot(tab.gelfingen.fin[,3],tab.gelfingen.fin[,2],type="l")

# do the same for the other location
tab.kleinwangen <- tab.kleinwangen[grepl("2019",tab.kleinwangen$Date),]
tab.kleinwangen.fin <- as.data.frame(cbind(as.numeric(c(nrow(tab.kleinwangen):1)),as.vector(tab.kleinwangen$Date),as.numeric(as.vector(tab.kleinwangen$Nombre.d.individus))))
tab.kleinwangen.fin[,1] <- as.numeric(as.character(tab.kleinwangen.fin[,1]))
tab.kleinwangen.fin[,3] <- as.numeric(as.character(tab.kleinwangen.fin[,3]))
head(tab.kleinwangen.fin)
colnames(tab.kleinwangen.fin) <- c("ID","date","count")
tab.kleinwangen.fin <- tab.kleinwangen.fin[order(tab.kleinwangen.fin$ID),]
head(tab.kleinwangen.fin)
plot(tab.kleinwangen.fin[,1],tab.kleinwangen.fin[,3],type="l")


### Now check the mines and egg phenology
tab.mines <- read.csv(paste(fl_nme,"/Extension_data/MinenEierPuppen.csv",sep=""),sep=";",header=T)
empty <- c(rep(NA,ncol(tab.mines)))
tab.mines <- rbind(empty,tab.mines[1:18,])
lines(1:nrow(tab.mines),tab.mines$Minen)
lines(1:nrow(tab.mines),tab.mines$Eier,col="red")
lines(1:nrow(tab.mines),tab.mines$Puppen,col="blue")

mines <- tab.mines$Minen_all/50
mines <- scale(tab.mines$Minen_all,center=1)
mines <- log10(tab.mines$Minen_all/50)

#############
#### plotting

## Now we can align our data with this data and plot it
locations <- c("Gel","Kle")
labfield <- c("Lab","Field")
letters <- c("a)","b)","c)","d)")

# now check my data on it
#t <- "all"
#c <- "FM"
dev.off()
correlations <- c()
par(mfrow=c(2,2),mar=c(4,4,2,5),xpd=F,oma=c(0.5,0.5,0,0))
r <- 0
for(l in locations){
  for(x in labfield){
    r <- r+1
    if(r %in% c(1,2)){plot(1:19,tab.gelfingen.fin$count,col="grey",type="n",lwd=2,lty=2, ylab=NA,xlab=NA,bty="n",xlim=c(0,20),ylim=c(0,300),yaxt="n",cex.axis=1.4,cex.lab=1.4)
    }else{plot(1:19,tab.gelfingen.fin$count,col="grey",type="n",lwd=2,lty=2, ylab=NA,xlab="week after April 15, 2019",bty="n",xlim=c(0,20),ylim=c(0,300),yaxt="n",cex.axis=1.4,cex.lab=1.4)}
    
    polygon(x=c(11,11,15,15),y=c(0,300,300,0),
            col = "lightgrey", border = NA,density = 8)
    polygon(x=c(5,5,9,9),y=c(0,300,300,0),
            col = "lightgrey", border = NA,density = 8)
    
    ## add arrows
    arrows(5,300,9,300, length = 0.05, lwd = 0.5)
    arrows(11,300,15,300, length = 0.05, lwd = 0.5)
    # put dates
    #abline(x=c(2.1,ylim=c(0,100), col="grey",cex=0.5,lty=4)
    par(xpd=FALSE)
    abline(v=10.5, col="grey",cex=0.5,lty=4)
    #abline(v=12.1, col="grey",cex=0.5,lty=4)
    par(xpd=TRUE)
    #text(x=2.1,y=-20,"May 1",font=3,cex=1)
    text(x=11,y=5,"July 1",font=3,cex=1)
    #text(x=12.1,y=-5,"August 1",font=3,cex=1.2)
    
    
    par(new=T)
    plot(1:nrow(tab.mines),mines,ylim=c(-1,1),col="blue",lwd=2,type="l",axes=F,xlab=NA,ylab=NA)
    axis(side = 2,line=0,cex.axis=1.4)
    if(r %in% c(1,3)){mtext(side = 2,"log mines per leaf",line = 3,cex=1.4)}
    #lines(1:nrow(tab.mines),tab.mines$Eier_C,col="red",ylim=c(min(tab.mines$Eier),max(tab.mines$Eier,lwd=2)))
    #lines(1:nrow(tab.mines),tab.mines$Puppen_C,col="blue",ylim=c(min(tab.mines$Puppen),max(tab.mines$Puppen)))
    par(new=T)
    plot(tab.gelfingen.fin[,3],tab.gelfingen.fin[,2],type="l",axes=F,xlab=NA,ylab=NA)
    
    # now check my data on it
    my.dat <- read.table(paste(fl_nme,"/Results/",x,"pics_22092020_probabilities_all/prct_",l,"_",t,".csv",sep=""),sep=";",header=T)
    if(x == "Field"){my.dat <- my.dat[2:20,]}
    my.dat <- cbind(my.dat[,c(1,3)],c(1:19))
    colnames(my.dat) <- c("date","prct","id.new")

    par(new=T)
    plot(my.dat[,"id.new"],my.dat[,"prct"],col="red",lwd=2,ylim=c(0,100),type="l",axes=F,xlab=NA,ylab=NA)
    axis(side = 4,line=0,cex.axis=1.4)
    if(r %in% c(2,4)){mtext(side=4,"% of leaves with mines",line=3,cex=1.4)}
    text(x=2,y=95,letters[r],cex=1.4)
    
    mines_na <- na.omit(mines)
    correlations <- c(correlations,cor(my.dat[,"prct"][7:19],mines_na))

   
   
  }
}

    
dev.print(png,paste(fl_nme,"/Graphs/Figure4_4paneLS.png",sep=""),height=600,width=800)


#plot legend
dev.off()
plot.new()
legend("bottomleft", legend=c("mine counts from DNN","mine counts from field"), lty=c(1,1), pch=c(NA, NA,NA), col=c("red", "blue"),cex=1.4,bty="n")
dev.print(png,paste(fl_nme,"/Graphs/Figure4_legend.png",sep=""),height=600,width=800)
