############################################################################
# Script to run the DNNs  ##################################################
############################################################################
# Marc Grünig 01.06.2021 ###################################################
############################################################################

library(reticulate)
library(tensorflow)
#install_tensorflow(version = 1.9, extra_packages = c("h5py", "pillow", "Scipy"))
# set configuration
config <- tf$ConfigProto(intra_op_parallelism_threads = 16L, inter_op_parallelism_threads = 16L)
library(keras)
session = tf$Session(config=config)
k_set_session(session)

### Start the script
# create a loop to do field and lab pics
loc <- c("field","lab")
for(l in loc){
  
  # loop over the 5-fold split
  for(k in 1:5){
    Cats <- c("Healthy","FM","PhysioDam","Lepidoptera","BrownSpots", "Mehltau", "All6")
    
    # loop over all categories
    for(cat in Cats){
      
      # comment for log file
      cat(paste("\n now Category \n", cat,sep=""))
      
      # If the category is "All6" we look at the categories independently, otherwise the focal category against all others    
      if(cat == "All6"){Categories <- c("Healthy","FM","PhysioDam","Lepidoptera","BrownSpots","Mehltau")
        }else{
          Categories <- c(paste(cat),"All")
      }
      
      # set the image size for import
      img_width <- 256
      img_height <- 256
      target_size <- c(img_width,img_height)
      channels <- 3
      output_n <- length(Categories)
      
      # paths to data
      if(cat == "All6"){
        train_image_files_path <- file.path(paste(l,"pics_cropped/K",k,"/Training/",sep=""))
        valid_image_files_path <- file.path(paste(l,"pics_cropped/K",k,"/Validation/",sep=""))
      }else{
        train_image_files_path <- file.path(paste(l,"pics_cropped/K",k,"/",cat,"_All/Training/",sep=""))
        valid_image_files_path <- file.path(paste(l,"pics_cropped/K",k,"/",cat,"_All/Training/",sep=""))
      }
      
      # set data augmentation
      train_data_gen <-  image_data_generator(
        rescale = 1/255,
        zoom_range = 0.4,
        rotation_range = 90,
        width_shift_range = 0.2,
        height_shift_range = 0.2,
        shear_range = 0.2,
        horizontal_flip = TRUE,
        vertical_flip = TRUE,
        fill_mode='constant'
      )
      
      # Validation data shouldn't be augmented! But it should also be scaled.
      valid_data_gen <- image_data_generator(
        rescale = 1/255
      ) 
      
      # training images
      train_image_array_gen <- flow_images_from_directory(train_image_files_path,
                                                          train_data_gen,
                                                          batch_size = 16,
                                                          target_size = c(img_width,img_height),
                                                          class_mode = 'categorical',
                                                          classes = Categories,
                                                          seed = 42)
      
      # validation images
      valid_image_array_gen <- flow_images_from_directory(valid_image_files_path,
                                                          valid_data_gen,
                                                          batch_size = 16,
                                                          target_size = c(img_width,img_height),
                                                          class_mode = 'categorical',
                                                          classes = Categories,
                                                          seed = 42)
      
      
      ### model definition ###
      # number of training samples
      train_samples <- train_image_array_gen$n
      # number of validation samples
      valid_samples <- valid_image_array_gen$n
      
      # define batch size and number of epochs
      batch_size <- 32
      epochs <- 100
      
      # download the resnet without top
      restnet <- application_resnet50(include_top = FALSE, weights = "imagenet",
                                      input_tensor = NULL, input_shape = c(img_width,img_height,3), pooling = NULL)
      
      # initialise model
      model <- keras_model_sequential()
      
      # first add the resnet
      model %>%
        restnet
      
      # add additional layers
      model %>%
        layer_dropout(0.5) %>%
        # and feed into dense layer
        layer_dense(256, kernel_regularizer = regularizer_l1_l2(l1 = 0.05, l2 = 0.05)) %>%
        layer_activation('relu') %>%
        layer_dropout(0.5) %>%

        # Outputs from dense layer are projected onto output layer
        layer_flatten() %>%
        layer_dense(output_n) %>%
        layer_activation('softmax')
      
      summary(model)
      
      # compile the model
      model %>% compile(
        loss = 'categorical_crossentropy',
        optimizer = optimizer_rmsprop(lr = 0.00001, decay = 1e-6),
        metrics = 'accuracy'
      )
      
      # create output directories
      dir.create(paste("model_outputs/",l,"pics/",sep=""))
      dir.create(paste("model_outputs/",l,"pics/K",k,sep=""))
      
      # fit
      hist <- fit_generator(
        model,
        # training data
        train_image_array_gen,
        # epochs
        steps_per_epoch = as.integer(train_samples / batch_size),
        epochs = epochs,
        # validation data
        validation_data = valid_image_array_gen,
        validation_steps = as.integer(valid_samples / batch_size),
        # print progress
        verbose = 1,
        workers = 4,
        callbacks = list(
          callback_early_stopping(monitor="val_loss",patience = 10,restore_best_weights = T),
          # save best model after every epoch
          callback_model_checkpoint(file.path(paste("model_outputs/",l,"pics/K",k,"_try/",l,"pics_",cat,".h5",sep="")), save_best_only = TRUE),
          # only needed for visualising with TensorBoard
          callback_tensorboard(log_dir = file.path(paste("model_outputs/",l,"pics/K",k,"_try/",l,"pics_",cat,"_logs",sep="")))
          
        )
        
      )
      history_df <- as.data.frame(hist)
      write.csv(history_df,paste("model_outputs/",l,"pics/K",k,"_try/",l,"pics_",cat,".csv",sep=""),sep=";")
      k_clear_session()
      gc()
    }
  } # close the CV loop
} # close the location loop