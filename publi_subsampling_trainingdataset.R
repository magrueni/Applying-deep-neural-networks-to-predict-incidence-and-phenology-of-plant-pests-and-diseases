############################################################################
# Script for phenology models correcting for temporal autocorrelation  #####
############################################################################
# Marc Grünig 01.06.2021 ###################################################
############################################################################

### In this script we do the subsampling of the manually categorized data for the training process
### We need to split into Training, Validation and Test dataset
### this script was used for Fieldpicss and labpics

#libraries
library(sp)

# path
fl_nme <- ""

# set seed for reproducability
set.seed(42)

# Categories
Categories <- c("Healthy","FM","BrownSpots","PhysioDam","Mehltau","Lepidoptera")

# loop over the categories
for(f in Categories){
  pics <- list.files(paste(fl_nme,"Fieldpics/",f,sep=""))
  
  #
  length(pics)
  tab1 <- as.data.frame(pics)
  tab <- cbind(1:nrow(tab1),tab1)

  subsample <- sample(1:5,size=nrow(tab),replace=TRUE,prob=c(rep(0.2,5)))
  tab.chunks <- cbind(tab,subsample)
  
  for(k in 1:5){
    tab.test <- tab.chunks[tab.chunks[,3] == k,]
    tab.rest <- tab.chunks[tab.chunks[,3] != k,]
    
    #subsample 10 % for validation
    subsample.val <- sample(1:2,size=nrow(tab.rest),replace=TRUE,prob=c(0.8,0.2))
    tab.chunks.val <- cbind(tab.rest,subsample.val)
    
    #put into train and valid chunks
    tab.train <- tab.chunks.val[tab.chunks.val[,4] == 1,]
    tab.val <- tab.chunks.val[tab.chunks.val[,4] == 2,]
    
    #save
    dir.create(paste(fl_nme,"Fieldpics/K",k,sep=""))
    
    dir.create(paste(fl_nme,"Fieldpics/K",k,"/Validation",sep=""))
    dir.create(paste(fl_nme,"Fieldpics/K",k,"/Validation/",f,sep=""))
    dir.create(paste(fl_nme,"Fieldpics/K",k,"/Training",sep=""))
    dir.create(paste(fl_nme,"Fieldpics/K",k,"/Training/",f,sep=""))
    dir.create(paste(fl_nme,"Fieldpics/K",k,"/Test",sep=""))
    dir.create(paste(fl_nme,"Fieldpics/K",k,"/Test/",f,sep=""))
    
    lapply(tab.val[,2],FUN=function(x){ 
      file.copy(paste(fl_nme,"Fieldpics/",f,"/",x,sep=""),paste(fl_nme,"Fieldpics/K",k,"/Validation/",f,"/",x,sep=""))
    })
    
    lapply(tab.train[,2],FUN=function(x){ 
      file.copy(paste(fl_nme,"Fieldpics/",f,"/",x,sep=""),paste(fl_nme,"Fieldpics/K",k,"/Training/",f,"/",x,sep=""))
    })
    
    lapply(tab.test[,2],FUN=function(x){ 
      file.copy(paste(fl_nme,"Fieldpics/",f,"/",x,sep=""),paste(fl_nme,"Fieldpics/K",k,"/Test/",f,"/",x,sep=""))
    })
    
  } #close k fold
  
} # close categories
