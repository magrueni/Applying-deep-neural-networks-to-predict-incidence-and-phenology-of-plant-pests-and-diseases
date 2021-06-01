############################################################################
# Script for plotting the performance of the DNNs in a boxplot #############
############################################################################
# Marc Grünig 01.06.2021  ##################################################
# cleaned ++++++++++++++ ###################################################
############################################################################

### plot the performance of the DNNs in a boxplot with the different categories on the x axes including field and standdardized pictures

# path
fl_nme <- ""

# categories
Cats <- c("Healthy","FM","PhysioDam","BrownSpots","Lepidoptera","Mehltau","All")

### We load the performance tables of labpics and fieldpics
## first do the Labpics
# create output
lab.output <- as.data.frame(matrix(nrow=length(Cats),ncol=4))
lab.output[is.na(lab.output)] <- 0
colnames(lab.output) <- c("prec","rec","f1","CA")
r <- 0
for(k in 1:5){
  
  if(!(file.exists(paste(fl_nme,"Results/PerformanceTables_Final/Results_K",k,"_Labpics.csv",sep="")))){
    cat(paste("CV", k, "file missing"))
    next
  }
  res <- read.csv(paste(fl_nme,"Results/PerformanceTables_Final/Results_K",k,"_Labpics.csv",sep=""),sep=";",header=T)
  colnames(res) <- c("prec","rec","f1","CA")
  
  lab.output <- lab.output + res[,1:4]
  assign(paste("lab.output",k,sep=""),res)
  r <- r + 1
}

lab.res <- lab.output/r
rownames(lab.res) <- Cats
lab.res


## now fieldpics
## first load all the evaluation tables
# create output
field.output <- as.data.frame(matrix(nrow=length(Cats),ncol=4))
field.output[is.na(field.output)] <- 0
colnames(field.output) <- c("prec","rec","f1","CA")
r <- 0
for(k in 1:5){
  
  if(!(file.exists(paste(fl_nme,"Results/PerformanceTables_Final/Results_K",k,"_Fieldpics.csv",sep="")))){
    cat(paste("CV", k, "file missing"))
    next
  }
  res <- read.csv(paste(fl_nme,"Results/PerformanceTables_Final/Results_K",k,"_Fieldpics.csv",sep=""),sep=";",header=T)
  colnames(res) <- c("prec","rec","f1","CA")
  
  field.output <- field.output + res[,1:4]
  assign(paste("field.output",k,sep=""),res)
  r <- r + 1
}

field.res <- field.output/r
rownames(field.res) <- Cats
field.res


## now combine all outputs
# transpose to combine the outputs of all 5 CV chunks
lab.metrics <- rbind(t(lab.output1[1:7,4]),t(lab.output2[1:7,4]),t(lab.output3[1:7,4]),t(lab.output4[1:7,4]),t(lab.output5[1:7,4]))
field.metrics <- rbind(t(field.output1[1:7,4]),t(field.output2[1:7,4]),t(field.output3[1:7,4]),t(field.output4[1:7,4]),t(field.output5[1:7,4]))

# add column to indicate lab or field picture
all.metrics <- rbind(lab.metrics,field.metrics)
all.metrics <- data.frame(all.metrics,c(rep("lab",nrow(lab.metrics)),rep("field",nrow(field.metrics))))
colnames(all.metrics) <- c("Undamaged","PLBM","Physical","Lepidoptera","Brown spots","Mildew","All","labfield")

### do the plot
dev.off()
par(oma=c(4,4,4,4),mar=c(2,2,2,2))
# create boxplot
boxplot(all.metrics[,c(1:7)], ylim = c(50,100),boxfill = NA, border = NA,cex.axis=1.5, xlab="Classes",ylab="Classification accuracy") #invisible boxes - only axes and plot area
#abline(h=80,col="grey",lty=2)
abline(h=85,col="grey",lty=2)
abline(h=90,col="grey",lty=2)
abline(h=95,col="grey",lty=2)
boxplot(all.metrics[all.metrics$labfield=="lab",-8], xaxt = "n", yaxt="n", add = TRUE, boxfill="red", 
        boxwex=0.3, at = c(1:7) - 0.2) #shift these left by -0.15
boxplot(all.metrics[all.metrics$labfield=="field", -8], xaxt = "n",yaxt="n", add = TRUE, boxfill="blue", 
        boxwex=0.3, at = c(1:7) + 0.2) #shift to the right by +0.15
legend("bottomright",c("standardized","field"),fill=c("red","blue"),bty="n",cex=1.5)
mtext(side = 1,"Classes",line = 3,cex=1.4)
mtext(side = 2,"Classification accuracy",line = 3,cex=1.4)


# save
dev.print(png,paste(fl_nme,"/Graphs/CrossValidation_results_lab.png",sep=""),height=800)

## do the same for F1 score
# transpose to combine the outputs of all 5 CV chunks
lab.metrics <- rbind(t(lab.output1[1:7,3]),t(lab.output2[1:7,3]),t(lab.output3[1:7,3]),t(lab.output4[1:7,3]),t(lab.output5[1:7,3]))
field.metrics <- rbind(t(field.output1[1:7,3]),t(field.output2[1:7,3]),t(field.output3[1:7,3]),t(field.output4[1:7,3]),t(field.output5[1:7,3]))

# add column to indicate lab or field picture
all.metrics <- rbind(lab.metrics,field.metrics)
all.metrics <- data.frame(all.metrics,c(rep("lab",nrow(lab.metrics)),rep("field",nrow(field.metrics))))
colnames(all.metrics) <- c("Undamaged","PLBM","Physical","Lepidoptera","Brown spots","Mildew","All","labfield")

dev.off()
# create boxplot
boxplot(all.metrics[,c(1:7)],ylim=c(0,1), boxfill = NA, border = NA,cex.axis=1.2) #invisible boxes - only axes and plot area
boxplot(all.metrics[all.metrics$labfield=="lab",-8], xaxt = "n", yaxt="n", add = TRUE, boxfill="red", 
        boxwex=0.25, at = c(1:7) - 0.15) #shift these left by -0.15
boxplot(all.metrics[all.metrics$labfield=="field", -8], xaxt = "n",yaxt="n", add = TRUE, boxfill="blue", 
        boxwex=0.25, at = c(1:7) + 0.15) #shift to the right by +0.15
legend("bottomright",c("standardized","field"),fill=c("red","blue"))
mtext(side = 1,"Classes",line = 3,cex=1.4)
mtext(side = 2,"F1-score",line = 3,cex=1.4)


# save
dev.print(png,paste(fl_nme,"/Graphs/CrossValidation_results_field.png",sep=""),height=700)

