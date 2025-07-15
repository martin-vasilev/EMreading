
#' Extract raw eye samples data from .asc files
#'
#' This function reads in data from .asc files and returns the raw eye samples in a 
#' data frame format (time, x position, y position, pupil size)
#'
#' @author Martin R. Vasilev
#'
#' @param data_list Input of data files to be processed. This can be specified in three ways:
#' 1) a directory that contains all the files (it will select all files ending with ".asc",
#' and order them by participant number, if present).
#' 2) Directory to a txt file that contains all the .asc data file names inside:
#' e.g., data_list= "C:/My Data/data_list.txt".
#' In the .txt file, the directory for each .asc data file should appear on a separate row,
#' e.g.: C:/My Data/subject1.asc /n
#' C:/My Data/subject2.asc
#' 3) A directory to a single .asc file: e.g., data_list= "C:/My Data/subject1.asc".
#' 
#' @param maxtrial Maximum number of trials in the experiment (default= 9999)
#' 
#' 
#' @include utility.R

ExtractSamples<- function(data_list= NULL, maxtrial= 9999){
  
  # check if user provided data dir:
  if(length(data_list)==0){
    data_list= file.choose() # make them chose a file
    message("To process multiple files, please specify a directory in 'data_list'")
  }
  
  # check file input:
  if(grepl('.txt', data_list)){
    data<- readLines(data_list, warn=F) # process multiple files
  }else{
    if(grepl('.asc', data_list)){ # if a single .asc file was provided...
      data<- data_list # process only 1 file
    } else{ # otherwise, it must be a dir of files
      data<- get_files(data_list)
    }
  }
  
  dat<- NULL
  
  
  for (i in 1:length(data)){ # for each subject..
    
    cat(sprintf("\nProcessing subject %i", i)); cat("\n")
    cat(sprintf("Loading data %s ...", data[i]));
    filename= data[i] #strsplit(data[i], "\\")
    
    file<- readLines(data[i]) # load file
    cat(" Done"); cat("\n")
    trial_db<- trial_info(file, maxtrial= maxtrial, selectEXP = T) # extract info about trials to be processed
    cat("Trial... ")
    
    for(j in 1:nrow(trial_db)){ # for each item
      
    cat(toString(j)); cat(" ")
    db<- trial_db[j,]
    
    start<- db$start
    end<- db$end
    
    if(is.na(end)){
      end<- length(file)
    }
  
    trialF<- file[start:end] # extract trial data:

    # remove messages and other flags:
    trialF<- trialF[!grepl("SFIX", trialF)]
    trialF<- trialF[!grepl("EFIX", trialF)]
    trialF<- trialF[!grepl("ESACC", trialF)]
    trialF<- trialF[!grepl("SSACC", trialF)]
    trialF<- trialF[!grepl("SBLINK", trialF)]
    trialF<- trialF[!grepl("EBLINK", trialF)]
    trialF<- trialF[!grepl("MSG", trialF)]
    trialF<- trialF[!grepl("END", trialF)]
    
    if(length(trialF)<1){
      
      cat('No samples- trial skipped!\n')
      next
    }
    
    # turn samples into a data frame:
    samples <-  as.data.frame(do.call( rbind, strsplit( trialF, '\t' ) ))
    
    # get rid of columns that are not needed
    samples$V5<- NULL
    samples$V6<- NULL
    
    # change numbers to numeric
    samples$V1<- as.numeric(samples$V1)
    samples$V2<- as.numeric(samples$V2)
    samples$V3<- as.numeric(samples$V3)
    samples$V4<- as.numeric(samples$V4)
    
    # change column labels:
    colnames(samples)<- c('time', 'xPos', 'yPos', 'pupil')
    
    # add trial info
    samples$sub<- i
    samples$cond<- db$cond[1]
    samples$item<- db$item[1]
    
    samples<- samples[, c(5,6,7,1,2,3,4)]
    
    if(length(samples)==0){
      next;
    }else{
      dat<- rbind(dat, samples) 
    }
    
  
    } # end of j
    
  } # end of i
  
  return(dat)
  
} # end of function
