############################################################################
# Script for cropping the training images ##################################
############################################################################
# Marc Grünig 01.06.2021 ###################################################
############################################################################

### After splitting the training data we can crop the images

library(imager)
library(raster)
library(filesstrings)
library(magick)

#set working directory
fl_nme <- ""

for(cv in 1:5){
# define chunks
  chnks <- c("Training","Validation")
  
  # create output folder
  dir.create(paste(fl_nme,"R_Virtualmachine/Chapter3/Labpics/Cropped",sep=""))
  
  # loop over the 3 chunks
  for(c in chnks){
    
    # list all folders in this subsection
    folders <- list.files(paste(fl_nme,"R_Virtualmachine/Chapter3/Labpics/",c,sep=""))
    
    #create output folder for this chunk
    dir.create(paste(fl_nme,"R_Virtualmachine/Chapter3/Labpics/Cropped/",c,sep=""))
    
    # loop over the folders
    for(k in folders){
      counter <- 0
      # loop over the treatment
      pics <- list.files(paste(fl_nme,"R_Virtualmachine/Chapter3/Labpics/",c,"/",k,sep=""))
      pics <- unique(pics)
      
      #create output folder for this category
      dir.create(paste(fl_nme,"R_Virtualmachine/Chapter3/Labpics/Cropped/",c,"/",k,sep=""))
      
      # apply cropping function for all pictures
      lapply(pics,FUN=function(x){
            if(x == "Thumbs.db"){x <- x}else{
              img <- image_read(paste(fl_nme,"R_Virtualmachine/Chapter3/Labpics/",c,"/",k,"/",x,sep=""))
              cropped <- image_crop(img,"2840x1560+500+300")
              #plot(cropped)
              counter <<- counter + 1
              print(counter)
              image_write(cropped, path = paste(fl_nme,"R_Virtualmachine/Chapter3/Labpics/Cropped/",c,"/",k,"/",x,".jpg",sep=""), format = "jpg")
              #file.rename(paste(fl_nme,"R_Virtualmachine/Chapter3/Pictures_cropped/",x,sep=""),paste(fl_nme,"R_Virtualmachine/Chapter3/Pictures_cropped/",Condition,"_",k,"_",Tr,"_",counter,".jpg",sep=""))
              #totalnr <- totalnr + 1
            }
          })
          gc()
          removeTmpFiles(h = 0.1)
          
      } # close folders loop
    
  } # close chunks
}
