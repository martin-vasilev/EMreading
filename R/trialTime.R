
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

trialTime<- function(data_list, maxtrial=999, startFlag= "SYNCTIME", endFlag= "DISPLAY OFF"){
  
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
    trial_db$start_time<- NA
    trial_db$end_time<- NA
    
    for(j in 1:nrow(trial_db)){
      
      trialFile<- file[trial_db$ID[j]:trial_db$end[j]]
      
      # start:
      startStr<- trialFile[which(grepl(startFlag, trialFile))]
      start<- get_num(startStr)
      trial_db$start_time[j]<- start
      
      # startStr<- file[trial_db$start[j]]
      # startStr= unlist(strsplit(startStr, " "))[1]
      # trial_db$start_time[j] <- get_num(startStr)
      
      # end:
      endStr<- trialFile[which(grepl(endFlag, trialFile))]
      end<- get_num(endStr)
      trial_db$end_time[j]<- end
      
      # endStr<- file[trial_db$end[j]-1]
      # endStr= unlist(strsplit(endStr, " "))[1]
      # trial_db$end_time[j]<- get_num(endStr)
    }
    
    trial_db$duration_ms<- trial_db$end_time- trial_db$start_time
    trial_db$sub<- i
    trial_db<- trial_db[, c("sub", "cond", "item", "seq", "start_time", "end_time", "duration_ms")]
    
    t<- rbind(t, trial_db)
  }
  
  
  return(t)
  
  cat("\n ALL DONE!")
}

