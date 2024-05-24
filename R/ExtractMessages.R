
#' Extract message timestamps data from .asc files
#'
#' This function reads in data from .asc files and returns the timestamps for when the message(s) occured.
#' It can extract an arbitrary number of flags per file.
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
#' @param message_name Name of the message flag in the data which needs to be extracted. If only one message
#' needs to be extracted, provide it as a string (e.g., message_name= "DISPLAY CHANGE STARTED"). If there is more
#' than one message, provide it as a vector of strings (e.g., message_name= c('DISPLAY CHANGE STARTED', 
#' 'DISPLAY CHANGE COMPLETED')).
#' 
#' @include utility.R

ExtractMessages<- function(data_list= NULL, maxtrial= 9999, message_name= "MSG"){
 
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
      
      
      for(k in 1:length(message_name)){
        
        temp<- data.frame(sub= NA, item= NA, cond= NA, seq= NA) # temp df with trial info
        var_name= message_name[k]
        temp[[var_name]]= NA # assign message name as new column in df
        
        trialFile<- file[trial_db$ID[j]:trial_db$end[j]]
        
        
        # basic info:
        temp$sub<- i # subject
        temp$item<- trial_db$item[j] # item
        temp$cond<- trial_db$cond[j] # condition
        temp$seq<- trial_db$seq[j]
        #dependnum<- trial_db$depend[j]
        
        
        msg_stamp<- trialFile[which(grepl(message_name[k], trialFile))]
        msg_stamp<- substr(msg_stamp, 1, unlist(gregexpr(' ', msg_stamp))[1])
        msg_time<- get_num(msg_stamp) 
        
        # add stamps to data frame:
        temp= temp[rep(seq_len(nrow(temp)), each = length(msg_time)), ]
        temp[[var_name]]<- msg_time
        
        dat<- rbind(dat, temp)
        
      }
      
      
     cat(toString(j)); cat(" ")
      
    } # end of j
  
  } # end of i
  
  return(dat)
  
} # end of fun
  