############################################################################
# Script for splitting the predicted probability output of the DNNs ########
############################################################################
# Marc Grünig 01.06.2021 ###################################################
# cleaned ++++++++++++++ ###################################################
############################################################################

# libraries
library(tidyverse)

# filepath
fl_nme <- ""

### Now split the dataset into different locations, treatments and so on
output_all <- read.table(paste(fl_nme,"/Results/Labpics_probabilities_all_cropped_K1.csv",sep=""),sep=";",header=T)

## Categories
Categories <- c("Healthy","FM","PhysioDam","BrownSpots","Lepidoptera","Mehltau")

#count how many pictures were predicted for the different classes
number.pred <- as.data.frame(cbind(nrow(output_all[output_all$Category == "Healthy",]),nrow(output_all[output_all$Category == "FM",]),nrow(output_all[output_all$Category == "PhysioDam",]),
                nrow(output_all[output_all$Category == "BrownSpots",]),nrow(output_all[output_all$Category == "Lepidoptera",]),nrow(output_all[output_all$Category == "Mehltau",])))
colnames(number.pred) <- Categories
 
# output folder to save it
dir.create(paste(fl_nme,"/Results/Labpics_probabilities_all",sep=""))
write.table(number.pred,paste(fl_nme,"/Results/Labpics_probabilities_all/numbers_predicted_classes.csv",sep=""))


### Then calculate the numbers for the different locations
# split up to the different locations
locations <- c("Gel","Kle")#,"Wae","Got")
output_Gel <- output_all[output_all$location == "Gel",]
output_Kle <- output_all[output_all$location == "Kle",]

# sort for treatments
# in the final analysis we don't separate the treatments
treatments <- c("all")#,"C","T","N")

# For both locations
for(l in locations[1:2]){
  output_loc <- get(paste("output",l,sep="_"))
  
  # sort for treatment
  for(t in treatments){
    if(t == "all"){output_loc_treat <- output_loc}else{output_loc_treat <- output_loc[output_loc$treatment == paste(t),]}
    
    # empty dataframe to store results
    prob_count <- as.data.frame(matrix(nrow=1,ncol = 8))
    
    # for all dates we get for each class the number of predicted images as probability
    for(d in as.numeric(levels(as.factor(output_loc_treat[,"date"])))){
      
      # first isolate all predictions from this count
      output_date <- output_loc_treat[output_loc_treat[,"date"] == d,]
      
      # check how many pictures were classified for this date
      nr_pics <- nrow(output_date)
      
      # then get the sum of the probability for each class
      sum_healthy <- sum(output_date[,"prob_healthy"])
      sum_fm <- sum(output_date[,"prob_fm"])
      sum_physiodam <- sum(output_date[,"prob_physiodam"])
      sum_lepi <- sum(output_date[,"prob_lepi"])
      sum_mehltau <- sum(output_date[,"prob_mehltau"])
      sum_brownspots <- sum(output_date[,"prob_brownspots"])
      
      # add it to the dataframe
      date_counts <- c(d,nr_pics,sum_healthy,sum_fm,sum_physiodam,sum_lepi,sum_mehltau,sum_brownspots)
      prob_count <- rbind(prob_count,date_counts)
    }
    
    # finalize dataframe
    colnames(prob_count) <- c("date","nr_pics","healthy_prob","fm_prob","physiodam_prob","lepi_prob","mehltau_prob","brownspots_prob")
    prob_count <- na.omit(prob_count)
    assign(paste("count",l,t,sep="_"),prob_count)
   
    # calculate the percentage of the predicted image for each class
    prob_prct <- as.data.frame(cbind(prob_count[,"date"],
                                    as.numeric(prob_count[,"healthy_prob"])/as.numeric(prob_count[,"nr_pics"])*100,
                                    as.numeric(prob_count[,"fm_prob"])/as.numeric(prob_count[,"nr_pics"])*100,
                                    as.numeric(prob_count[,"physiodam_prob"])/as.numeric(prob_count[,"nr_pics"])*100,
                                    as.numeric(prob_count[,"brownspots_prob"])/as.numeric(prob_count[,"nr_pics"])*100,
                                    as.numeric(prob_count[,"lepi_prob"])/as.numeric(prob_count[,"nr_pics"])*100,
                                    as.numeric(prob_count[,"mehltau_prob"])/as.numeric(prob_count[,"nr_pics"])*100,
                                    c(1:nrow(prob_count))))
    
    colnames(prob_prct) <- c("date","healthy_prob","fm_prob","pyhsiodam_prob","brownspots_prob","lepi_prob","mehltau_prob","id")
    assign(paste("count_prct",l,t,sep="_"),prob_prct)
    write.table(prob_prct,paste(fl_nme,"/Results/Labpics_probabilities_all/prct_",l,"_",t,".csv",sep=""),sep=";")
  }
}

######## Fieldpics
# do the same for fieldpics

### Now split the dataset into different locations, treatments and so on
output_all <- read.table(paste(fl_nme,"/Results/Fieldpics_probabilities_all_cropped_K1.csv",sep=""),sep=";",header=T)

## Categories
Categories <- c("Healthy","FM","PhysioDam","BrownSpots","Lepidoptera","Mehltau")

#count how many pictures were predicted for the different classes
number.pred <- as.data.frame(cbind(nrow(output_all[output_all$Category == "Healthy",]),nrow(output_all[output_all$Category == "FM",]),nrow(output_all[output_all$Category == "PhysioDam",]),
                                   nrow(output_all[output_all$Category == "BrownSpots",]),nrow(output_all[output_all$Category == "Lepidoptera",]),nrow(output_all[output_all$Category == "Mehltau",])))
colnames(number.pred) <- Categories

# output folder to save it
dir.create(paste(fl_nme,"/Results/Fieldpics_probabilities_all",sep=""))
write.table(number.pred,paste(fl_nme,"/Results/Fieldpics_probabilities_all/numbers_predicted_classes.csv",sep=""))


### Then calculate the numbers for the different locations
# split up to the different locations
locations <- c("Gel","Kle")
output_Gel <- output_all[output_all$location == "Gel",]
output_Kle <- output_all[output_all$location == "Kle",]

# sort for treatments
# in the final analysis we don't separate the treatments
treatments <- c("all")#,"C","T","N")

# For both locations
for(l in locations[1:2]){
  output_loc <- get(paste("output",l,sep="_"))
  #output_loc %>% group_by(date) %>% count(date)
  
  # sort for treatment
  for(t in treatments){
    if(t == "all"){output_loc_treat <- output_loc}else{output_loc_treat <- output_loc[output_loc$treatment == paste(t),]}
    
    # empty dataframe to store results
    prob_count <- as.data.frame(matrix(nrow=1,ncol = 8))
    
    # for all dates we get for each class the number of predicted images as probability
    for(d in as.numeric(levels(as.factor(output_loc_treat[,"date"])))){
      
      # first isolate all predictions from this count
      output_date <- output_loc_treat[output_loc_treat[,"date"] == d,]
      
      # check how many pictures were classified for this date
      nr_pics <- nrow(output_date)
      
      # then get the sum of the probability for each class
      sum_healthy <- sum(output_date[,"prob_healthy"])
      sum_fm <- sum(output_date[,"prob_fm"])
      sum_physiodam <- sum(output_date[,"prob_physiodam"])
      sum_lepi <- sum(output_date[,"prob_lepi"])
      sum_mehltau <- sum(output_date[,"prob_mehltau"])
      sum_brownspots <- sum(output_date[,"prob_brownspots"])
      
      # add it to the dataframe
      date_counts <- c(d,nr_pics,sum_healthy,sum_fm,sum_physiodam,sum_lepi,sum_mehltau,sum_brownspots)
      prob_count <- rbind(prob_count,date_counts)
    }
    
    # finalize dataframe
    colnames(prob_count) <- c("date","nr_pics","healthy_prob","fm_prob","physiodam_prob","lepi_prob","mehltau_prob","brownspots_prob")
    prob_count <- na.omit(prob_count)
    assign(paste("count",l,t,sep="_"),prob_count)
    
    # calculate the percentage of the predicted image for each class
    prob_prct <- as.data.frame(cbind(prob_count[,"date"],
                                     as.numeric(prob_count[,"healthy_prob"])/as.numeric(prob_count[,"nr_pics"])*100,
                                     as.numeric(prob_count[,"fm_prob"])/as.numeric(prob_count[,"nr_pics"])*100,
                                     as.numeric(prob_count[,"physiodam_prob"])/as.numeric(prob_count[,"nr_pics"])*100,
                                     as.numeric(prob_count[,"brownspots_prob"])/as.numeric(prob_count[,"nr_pics"])*100,
                                     as.numeric(prob_count[,"lepi_prob"])/as.numeric(prob_count[,"nr_pics"])*100,
                                     as.numeric(prob_count[,"mehltau_prob"])/as.numeric(prob_count[,"nr_pics"])*100,
                                     c(1:nrow(prob_count))))
    
    colnames(prob_prct) <- c("date","healthy_prob","fm_prob","pyhsiodam_prob","brownspots_prob","lepi_prob","mehltau_prob","id")
    assign(paste("count_prct",l,t,sep="_"),prob_prct)
    write.table(prob_prct,paste(fl_nme,"/Results/Fieldpics_probabilities_all/prct_",l,"_",t,".csv",sep=""),sep=";")
  }
}

###########################
### plotting
###########################

# fm for both locations
r <- 0
for(l in locations[1:2]){
  r <- r + 1
  dat <- read.table(paste(fl_nme,"/Results/Labpics_probabilities_all/prct_",l,"_all.csv",sep=""),sep=";",header=T)

  if(r == 1){plot(dat$id,dat$fm_prob,type="l",col="red",xlab="week",ylab="% damaged leaves",ylim=c(0,100),xlim=c(0,20))
  }else{lines(dat$id,dat$fm_prob,col="blue")}
  
  # Fieldpics
  dat <- read.table(paste(fl_nme,"/Results/Fieldpics_probabilities_all/prct_",l,"_all.csv",sep=""),sep=";",header=T)
  dat <- dat[-1,]
  dat$id <- c(1:nrow(dat))

  if(l == "Gel"){lines(dat$id,dat$fm_prob,col="red",lty=2)}else{lines(dat$id,dat$fm_prob,col="blue",lty=2)}
  
}
legend("topleft",c("Gelfingen standardized","Gelfingen field","Kleinwangen standardized", "Kleinwangen field"),lty=c(1,2,1,2),col=c("red","red","blue","blue"))
dev.print(png,paste(fl_nme,"/Graphs/Phenologycurve_FM.png",sep=""),height=500)



r <- 0
for(l in locations[1:2]){
  r <- r + 1
  dat <- read.table(paste(fl_nme,"/Results/Fieldpics_19042020_probabilities/prct_FM_",l,"_C.csv",sep=""),sep=";",header=T)
  if(r == 1){plot(dat[,5],dat[,4],type="l",col="red",xlab="week",ylab="% damaged leafs",ylim=c(0,100))
  }else{lines(dat[,5],dat[,4],col="blue")}
  dat <- read.table(paste(fl_nme,"/Results/Fieldpics_19042020_probabilities/prct_FM_",l,"_T.csv",sep=""),sep=";",header=T)
  if(l == "Gel"){lines(dat[,5],dat[,4],col="red",lty=2)}else{lines(dat[,5],dat[,4],col="blue",lty=2)}
}
legend("topleft",c("Gelfingen treated","Gelfingen control","Kleinwangen treated", "Kleinwangen control"),lty=c(1,2,1,2),col=c("red","red","blue","blue"))
dev.print()



