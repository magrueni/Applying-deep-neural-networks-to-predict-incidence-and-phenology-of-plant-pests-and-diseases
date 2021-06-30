############################################################################
# Script using the DNN to predict the images into classes ##################
############################################################################
# Marc Grünig 01.06.2021 ###################################################
############################################################################

### After the evaluation of the DNNs with the 5-CV we can take the best performing Network and predict the rest of the dataset

# libraries
library(reticulate)
library(tensorflow)

# limit cores
config <- tf$ConfigProto(intra_op_parallelism_threads = 1L, inter_op_parallelism_threads = 1L)
library(keras)
session = tf$Session(config=config)
k_set_session(session)
library(magick)
library(plyr)
library(dplyr)
library(tidyverse)

# path
fl_nme <- ""

#load network
new_modelx <- load_model_hdf5(paste(fl_nme, "/ModelsCluster/ModelOutputs_CV/Labpics/cropped/K4/Labpics_All6.h5",sep="/"))
#summary(new_modelx)

# load images
Categories <- c("Healthy","FM","PhysioDam","Lepidoptera","Mehltau","BrownSpots")

img_width <- 256
img_height <- 256
target_size <- c(img_width,img_height)
channels <- 3
output_n <- length(Categories)

# path to the pics that need to be predicted
# We classify all the pictures that have not been use 
path_to_pred <- file.path(paste(fl_nme,"pictures_to_predict/Cropped/Labpics_ToPred/",sep=""))
pics_topred <- list.files(path_to_pred)

# create putput file
output_table <- as.data.frame(matrix(nrow = length(pics_topred),ncol = 12))
colnames(output_table) <- c("Name","PredictionClass","Category","location","date","treatment","prob_healthy","prob_fm","prob_physiodam","prob_brownspots","prob_lepi","prob_mehltau")

# counter
r <- 0

# loop over the pics
for(pic in pics_topred){
  if(is.na(pic)){next}
  if(pic == "Thumbs.db"){next}
  r <- r+1
  if((r/1000) %in% c(1:100)){print(r)}
  if(file.exists(paste(path_to_pred,pic,sep=""))){
    
    # load image
    img.keras <- keras::image_load(paste(path_to_pred,pic,sep=""),
                                   target_size = c(img_height, img_width), 
                                   grayscale = FALSE)
  }else{next}
  
  # make the prediction
  x <- image_to_array(img.keras)/255
  x.new <- array_reshape(x,c(1,dim(x)))
  pred <- predict_classes(new_modelx,x.new)
  pred.prob <- predict_proba(new_modelx,x.new)
  cat <- Categories[pred+1]
  
  # isolate place, date and treatment
  pic.nam <- gsub("Kleinwangen","Kle",pic)
  pic.nam <- gsub("Gelfingen","Gel",pic.nam)
  pic.nam <- gsub("Waedenswil","Wae",pic.nam)
  pic.nam <- gsub("Gottshalde","Got",pic.nam)
  
  # get the meta data of the image
  location <- substr(pic.nam,3,5)
  dd <- substr(pic.nam,7,8)
  mm <- substr(pic.nam,9,10)
  yyyy <- substr(pic.nam,11,14)
  date <- paste(yyyy,mm,dd,sep="")
  treatment <- substr(pic.nam,16,16)
  
  # put all together
  values <- c(pic.nam,pred,cat,location,date,treatment,as.vector(pred.prob))
  # put data in output dataframe
  output_table[r,1:12] <- values
}

# clean and save
#check output and loose the NAs
ouput <- na.omit(output_table[,1:12])
write.table(ouput,paste(fl_nme,"/Results/Labpicsprobabilities_all_cropped.csv",sep=""),sep=";")

####################################
### same for fieldpics
####################################

#load network
new_modelx <- load_model_hdf5(paste(fl_nme, "/ModelsCluster/ModelOutputs_CV/Fieldpics/cropped/K1/Fieldpics_All6.h5",sep="/"))
#summary(new_modelx)

# load images
Categories <- c("Healthy","FM","PhysioDam","Lepidoptera","Mehltau","BrownSpots")

img_width <- 256
img_height <- 256
target_size <- c(img_width,img_height)
channels <- 3
output_n <- length(Categories)


path_to_pred <- file.path(paste(fl_nme,"pictures_to_predict/Cropped/Fieldpics_ToPred/",sep=""))
pics_topred <- list.files(path_to_pred)

####
output_table <- as.data.frame(matrix(nrow = length(pics_topred),ncol = 12))
colnames(output_table) <- c("Name","PredictionClass","Category","location","date","treatment","prob_healthy","prob_fm","prob_physiodam","prob_brownspots","prob_lepi","prob_mehltau")

r <- 0


for(pic in pics_topred){
  if(is.na(pic)){
    cat(paste("pic is NA", pic,sep=" "))
    next}
  if(pic == "Thumbs.db"){next}
  if(grepl("Waedenswil",pic)){next}
  if(grepl("Gottshalde",pic)){next}
  r <- r+1
  if((r/1000) %in% c(1:100)){print(r)}
  if(file.exists(paste(path_to_pred,pic,sep=""))){
    #img <- image_read(paste(path_to_pred,pic,sep=""))
    img.keras <- keras::image_load(paste(path_to_pred,pic,sep=""),
                                   target_size = c(img_height, img_width), 
                                   grayscale = FALSE)
  }else{
    cat(paste("file not existing", pic,sep=" "))
    next}
  x <- image_to_array(img.keras)/255
  x.new <- array_reshape(x,c(1,dim(x)))
  pred <- predict_classes(new_modelx,x.new)
  pred.prob <- predict_proba(new_modelx,x.new)
  cat <- Categories[pred+1]
  
  # isolate place, date and treatment
  pic.nam <- gsub("Kleinwangen","Kle",pic)
  pic.nam <- gsub("Gelfingen","Gel",pic.nam)
  pic.nam <- gsub("Waedenswil","Wae",pic.nam)
  pic.nam <- gsub("Gottshalde","Got",pic.nam)
  
  location <- substr(pic.nam,3,5)
  dd <- substr(pic.nam,7,8)
  mm <- substr(pic.nam,9,10)
  yyyy <- substr(pic.nam,11,14)
  date <- paste(yyyy,mm,dd,sep="")
  treatment <- substr(pic.nam,16,16)
  # put all together
  values <- c(pic.nam,pred,cat,location,date,treatment,as.vector(pred.prob))
  # put data in output dataframe
  output_table[r,1:12] <- values
}

#check output and loose the NAs
ouput <- na.omit(output_table)
#nrow(ouput)
write.table(ouput,paste(fl_nme,"/Results/Fieldpicsprobabilities_all_cropped.csv",sep=""),sep=";")


