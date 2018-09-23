
#' Extracts trial times from raw .asc files
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
#' @param maxtrial Maximum number of trials in the experiment
#' 
#' @include utility.R
#' 

trialTime<- function(data_list, maxtrial){
  
  t<- NULL
  
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
  
  cat("Processing subject: ")
  
  for (i in 1:length(data)){
    cat(i); cat(" ")
    
    file<- readLines(data[i]) # load file
    trial_db<- trial_info(file, maxtrial) # extract trial info 
    trial_db$duration_ms<- trial_db$end- trial_db$start
    trial_db$sub<- i
    trial_db<- trial_db[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)]
    
    t<- rbind(t, trial_db)
  }
  
  
  return(t)
  
  cat("\n ALL DONE!")
}

