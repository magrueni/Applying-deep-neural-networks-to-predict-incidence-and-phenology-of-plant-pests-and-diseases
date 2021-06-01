############################################################################
# Script for collecting the pictures of different classes ##################
############################################################################
# Marc Grünig 01.06.2021 ###################################################
# cleaned ++++++++++++++ ###################################################
############################################################################

### In this script I cropped the pictures to a smaller extent in order to focus on the leaves

# libraries
library(imager)
library(raster)
library(filesstrings)
library(magick)

#set working directory
fl_nme <- ""

# picture settings
cond <- c("Field","Controlled")
place <- c("Waedenswil","Kleinwangen","Gelfingen")
treat <- c("behandelt","unbehandelt")

# counter1
totalnr <- 0

# for both conditions - these are the two main folders
for(c in cond){
  if(c == "Field"){Condition <- "F"}else{Condition <- "L"} # naming the file with F for field or L for lab
  # list all folders in this subsection
  folders <- list.files(paste(fl_nme,"Chapter3/PicturesR/",c,sep=""))
  # loop over the folders
  for(k in folders){
    # counter2
    counter <- 0
    # loop over the treatment
    pics <- list.files(paste(fl_nme,"Chapter3/PicturesR/",c,"/",k,sep=""))
    pics <- unique(pics)
    if(length(pics) < 4){
      for(t in treat){
        
        if(t == "behandelt"){Tr <- "T"}else{Tr <- "C"} # using T for Treatment and C for Control
        # list all pictures in this folder
        pics <- list.files(paste(fl_nme,"Chapter3/PicturesR/",c,"/",k,"/",t,sep=""))
        # do the same for all pictures
        counter <- 0
        lapply(pics,FUN=function(x){
          if(x == "Thumbs.db"){x <- x}else{
          img <- image_read(paste(fl_nme,"Chapter3/PicturesR/",c,"/",k,"/",t,"/",x,sep=""))
          cropped <- image_crop(img,"2840x1560+500+300")
          #plot(cropped)
          counter <<- counter + 1
          print(counter)
          image_write(cropped, path = paste(fl_nme,"R_Virtualmachine/Chapter3/Pictures_cropped/",Condition,"_",k,"_",Tr,"_",counter,".jpg",sep=""), format = "jpg")
          totalnr <- totalnr + 1
        }
          })
        gc()
        removeTmpFiles(h = 0.1)
      } # close treatment loop
    }else{
      # do the same for all pictures
      Tr <- "N" #state neutral for treatment because no treatment was evaluated
      counter <- 0
      lapply(pics,FUN=function(x){ 
        counter <<- counter + 1
        print(counter)
        img <- image_read(paste(fl_nme,"Chapter3/PicturesR/",c,"/",k,"/",x,sep=""))
        cropped <- image_crop(img,"2840x1560+500+300")
        image_write(cropped, path = paste(fl_nme,"R_Virtualmachine/Chapter3/Pictures_cropped/",Condition,"_",k,"_",Tr,"_",counter,".jpg",sep=""), format = "jpg")
        totalnr <<- totalnr + 1
      })
      gc()
      removeTmpFiles(h = 0.1)
    }
    print(paste("The total number is: ",totalnr,sep=""))
  } # close folders
  
} # close conditions
