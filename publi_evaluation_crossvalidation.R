############################################################################
# Script for evaluating the cross validation DNNs ##########################
############################################################################
# Marc Grünig 22.10.2020 ###################################################
############################################################################

# libraries
library(reticulate)
library(tensorflow)
#install_tensorflow(version = 1.9)

# limit cores
config <- tf$ConfigProto(intra_op_parallelism_threads = 1L, inter_op_parallelism_threads = 1L)
library(keras)
session = tf$Session(config=config)
k_set_session(session)
library(magick)

# path
fl_nme <- ""

# Categories we are looking at  ### IMPORTANT: should be the same order as in the model
Cats <- c("Healthy","FM","PhysioDam","Lepidoptera","BrownSpots","Mehltau","All6")

# 5 fold crossvalidation -> loop over the 5 chunks
for(k in 1:5){
  
  # create output table
  output.table <- as.data.frame(matrix(ncol=4,nrow=length(Cats)))
  rownames(output.table) <- Cats
  
  # loop over the categories to evaluate the performance of the different classification tasks
  for(cc in Cats){
    
    # load DNN
    new_modelx <- load_model_hdf5(paste(fl_nme, "ModelsCluster/ModelOutputs_CV/Fieldpics/cropped/K",k,"/Fieldpics_",cc,".h5",sep=""))
    #summary(new_modelx)
    
    # load images
    Categories <- c(paste(cc),"All")
    if(cc == "All6"){Categories <- c("Healthy","FM","PhysioDam","Lepidoptera","BrownSpots","Mehltau")}
    
    # set image configurations
    img_width <- 256
    img_height <- 256
    target_size <- c(img_width,img_height)
    channels <- 3
    output_n <- length(Categories)
    
    # path to valid images
    valid_image_files_path <- file.path(paste(fl_nme,"Fieldpics/K",k,"/cropped/Test/",sep=""))
    
    # file path for each category
    for(c in Categories){
      path <- file.path(paste(fl_nme,"Fieldpics/K",k,"/cropped/Test/",c,"/",sep=""))
      assign(paste(c,"path",sep="_"),path)
    }
    
    
    # define some counters
    r <- 0
    f <- 0
    
    # empty vectors and dataframes to count the classified pictures
    wrong.preds <- list()
    all.pics <- c()
    predAct.df <- as.data.frame(matrix(ncol=2,nrow=0))
    colnames(predAct.df) <- c("pred","act")
    
    # Then predict for each category
    for(c in Categories){
      
      # If the category is "All" we collect the images of the different folders
      if(c == "All"){
        pics_cat <- c()
        foldrs <- list.files(valid_image_files_path)
        
        for(foldr in foldrs){
          if(foldr %in% Categories){next}
          pics.foldr <- list.files(paste(valid_image_files_path,foldr,sep=""))
          
          all.pics <- c(all.pics,length(pics.foldr))
          for(files in pics.foldr){
            if(files == "Thumbs.db"){next}
            f <- f + 1
            
            # load the image and prepare to the same format as the DNN is used to
            img.keras <- keras::image_load(paste(valid_image_files_path,foldr,"/",files,sep=""),
                                           target_size = c(img_height, img_width), 
                                           grayscale = FALSE)
            x <- image_to_array(img.keras)/255
            x.new <- array_reshape(x,c(1,dim(x)))
            
            # make the prediction
            pred <- predict_classes(new_modelx,x.new)
            
            # save the prediction and the actual class in the dataframe
            predAct.df[f,1] <- Categories[pred+1]
            predAct.df[f,2] <- c
            
            # if the predicted class is not the actual class we visualize the image
            if(Categories[pred+1] != c){
              wrong.preds <- c(wrong.preds,paste(get(paste(c,"_path",sep="")),files,sep=""))
              r <- r + 1
              #plot the wrong image
              img <- image_read(paste(valid_image_files_path,foldr,"/",files,sep=""))
              plot(img)
              text(x=100,y=300,paste("Category: ",c,files,sep=""))
              text(x=100,y=100,paste("Prediction: ",Categories[pred+1],sep=""))
              
              print(paste("wrong prediction! class: ",c, "; predicted: ",Categories[pred+1],"; ",files,sep=""))
            }
          }
        }
      
      # if the category is not "All" we just get the images from the folder of the class and do the same as before
      }else{
        
        pics_cat <- list.files(paste(valid_image_files_path,c,sep="/"))
        all.pics <- c(all.pics,length(pics_cat))
        for(files in pics_cat){
          if(files == "Thumbs.db"){next}
          f <- f + 1
          img.keras <- keras::image_load(paste(valid_image_files_path,c,files,sep="/"),
                                         target_size = c(img_height, img_width), 
                                         grayscale = FALSE)
          x <- image_to_array(img.keras)/255
          x.new <- array_reshape(x,c(1,dim(x)))
          pred <- predict_classes(new_modelx,x.new)
          
          predAct.df[f,1] <- Categories[pred+1]
          predAct.df[f,2] <- c
          
          if(Categories[pred+1] != c){
            wrong.preds <- c(wrong.preds,paste(get(paste(c,"_path",sep="")),files,sep=""))
            r <- r + 1
            #plot the wrong image
            #img <- image_read(paste(paste(get(paste(c,"_path",sep="")),files,sep="")))
            #plot(img)
            #text(x=100,y=300,paste("Category: ",c,files,sep=""))
            #text(x=100,y=100,paste("Prediction: ",Categories[pred+1],sep=""))
            
            print(paste("wrong prediction! class: ",c, "; predicted: ",Categories[pred+1],"; ",files,sep=""))
          }
        }
      }
    }
    
    # clean the df
    predAct <- na.omit(predAct.df)
    
    ## now we can calculate the evaluation metrics
    for(c in Categories){
      # get confusion matrix values
      TP <- nrow(predAct[predAct[,1] == c & predAct[,2] == c,])
      FP <- nrow(predAct[predAct[,1] == c & predAct[,2] != c,])
      FN <- nrow(predAct[predAct[,2] == c & predAct[,1] != c,])
      
      # calculate metrics
      precision <-  TP / (TP + FP)
      recall <- TP / (TP + FN)
      Fscore_rev <- 2 * precision * recall / (precision + recall)
      CA <- 100-100/nrow(predAct)*sum(FP+FN)
      
      # save
      assign(paste("precision.",c,sep=""),precision)
      assign(paste("recall.",c,sep=""),recall)
      assign(paste("Fscore_rev.",c,sep=""),Fscore_rev)
      assign(paste("CA.",c,sep=""),CA)
    }
    
    
    total <- c()
    weighted.precision <- c()
    for(c in Categories){
      prec <- get(paste("precision.",c,sep=""))
      weight <- nrow(predAct[predAct[,2] == c,])
      weighted.precision <- c(weighted.precision,prec*weight)
      total <- c(total,weight)
    }
    precision.score <- sum(weighted.precision)/sum(total)
    
    
    total <- c()
    weighted.recall <- c()
    for(c in Categories){
      recall <- get(paste("recall.",c,sep=""))
      weight <- nrow(predAct[predAct[,2] == c,])
      weighted.recall <- c(weighted.recall,recall*weight)
      total <- c(total,weight)
    }
    recall.score <- sum(weighted.recall)/sum(total)
    recall.score
    
    total <- c()
    weighted.f1 <- c()
    for(c in Categories){
      f1 <- get(paste("Fscore_rev.",c,sep=""))
      weight <- nrow(predAct[predAct[,2] == c,])
      weighted.f1 <- c(weighted.f1,f1*weight)
      total <- c(total,weight)
    }
    f1.score <- sum(weighted.f1)/sum(total)
    
    total <- c()
    weighted.CA <- c()
    for(c in Categories){
      CA <- get(paste("CA.",c,sep=""))
      weight <- nrow(predAct[predAct[,2] == c,])
      weighted.CA <- c(weighted.CA,CA*weight)
      total <- c(total,weight)
    }
    CA.score <- sum(weighted.CA)/sum(total)
    
    output.table[cc,1:4] <- c(get(paste("precision.",Categories[1],sep="")),get(paste("recall.",Categories[1],sep="")),get(paste("Fscore_rev.",Categories[1],sep="")),get(paste("CA.",Categories[1],sep="")))
    
  }#close category
  assign(paste("Results_K",k,sep=""),output.table)
  write.table(output.table, paste(fl_nme,"/Results/PerformanceTables_Final/Results_K",k,"_Fieldpics.csv",sep=""),sep=";")
}#close CV part


final.output <- as.data.frame(matrix(nrow=length(Cats),ncol=4))
final.output[is.na(final.output)] <- 0
colnames(final.output) <- c("prec","rec","f1")
for(k in 1:5){
  
  res <- read.csv(paste(fl_nme,"Results/PerformanceTables_Final/Results_K",k,"_Fieldpics.csv",sep=""),sep=";",header=T)
  colnames(res) <- c("prec","rec","f1")
  
  final.output <- final.output + res[,1:4]
  assign(paste("output",k,sep=""),res)
}

final.res <- final.output/5
rownames(final.res) <- Cats
final.res

val <- c()
for(k in 1:5){
  k.val <- get(paste("output",k,sep=""))
  val <- c(val,k.val[7,4])
}

mean(val)
sd(val)
