############################################################################
# Script for subsampling images ############################################
############################################################################
# Marc Grünig 01.06.2021 ###################################################
# cleaned ++++++++++++++ ###################################################
############################################################################

### in this script I randomly subsample images which I then manually classified and used to train DNNs

# libraries
library(sp)

# working directory
fl_nme <- ""

# set seed for reproducability
set.seed(42)

### look into the folder and list all files in there
folders <- list.files(paste(fl_nme,"R_Virtualmachine/Chapter3/Categories/",sep=""))

tab <- as.data.frame(matrix(ncol=1))
colnames(tab) <- c("pics")
for(f in folders){
  pics <- list.files(paste(fl_nme,"R_Virtualmachine/Chapter3/Categories/",f,sep=""))
  tab1 <- as.data.frame(pics)
  tab <- rbind(tab,tab1)
}

# I save a table with all those images
write.table(tab,paste(fl_nme,"R_Virtualmachine/Chapter3/categorizedPics.csv",sep=""),sep = ";")

### sample from all pics
tab <- na.omit(read.table(paste(fl_nme,"R_Virtualmachine/Chapter3/categorizedPics.csv",sep=""),sep = ";",header=T))
all.pics <- list.files(paste(fl_nme,"R_Virtualmachine/Chapter3/PicturesRenamed4/",sep=""))

# I subsample randomly
subsample <- sample(all.pics, 10000, replace = T)

# create the output folder
dir.create(paste(fl_nme,"R_Virtualmachine/Chapter3/ToCat/",sep=""))

# then copy the randomly sampled pictures to this folder
lapply(subsample,FUN=function(x){ 
  file.copy(paste(fl_nme,"R_Virtualmachine/Chapter3/PicturesRenamed4/",x,sep=""),paste(fl_nme,"R_Virtualmachine/Chapter3/ToCat/",x,sep=""))
})



