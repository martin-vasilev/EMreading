
#' Extract question data from .asc files
#'
#' This function reads in data from question trials and calculates accuracy, response time, etc.
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
#' @param Correct_Answ_Flag Name of the message flag in the data which indicates what the correct answer was 
#' (default= "QUESTION_ANSWER"; Umass convention)
#' 
#' @param Answ_Button_Flag Name of the message flag in the data which indicates the answer button press 
#' (default= "ENDBUTTON"; Umass convention)
#' 
#' @param summary a logical indicating whether to print average accuracy per subject (default= TRUE)
#' 
#' @include utility.R

Question<- function(data_list= NULL, maxtrial= 9999, Correct_Answ_Flag= "QUESTION_ANSWER",
                    Answ_Button_Flag= "ENDBUTTON", summary= TRUE){
  
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
  
  Quest<- NULL
  
  if(summary){
    Sub_acc= NULL
  }
  
  for (i in 1:length(data)){ # for each subject..
    
    cat(sprintf("\nProcessing subject %i", i)); cat("\n")
    cat(sprintf("Loading data %s ...", data[i]));
    filename= data[i] #strsplit(data[i], "\\")
    
    file<- readLines(data[i]) # load file
    cat(" Done"); cat("\n")
    trial_db<- trial_info(file, maxtrial= maxtrial, selectEXP = F) # extract info about trials to be processed
    cat("Trial... ")
    
    for(j in 1:nrow(trial_db)){ # for each item
      
      trialFile<- file[trial_db$ID[j]:trial_db$end[j]]
      
      # basic info:
      
      sub<- i # subject
      item<- trial_db$item[j] # item
      cond<- trial_db$cond[j] # condition
      seq<- trial_db$seq[j]
      dependnum<- trial_db$depend[j]
      
      # What was the correct answer?:
      correctresp<- trialFile[which(grepl(Correct_Answ_Flag, trialFile))]
      correctresp<-  substr(correctresp, regexpr(Correct_Answ_Flag,
                           correctresp)[1] +nchar(Correct_Answ_Flag), nchar(correctresp))
      correctresp<- get_num(correctresp)
      
      # What button did the participant press?
      subresp<- trialFile[which(grepl(Answ_Button_Flag, trialFile))]
      subresp<- substr(subresp, regexpr(Answ_Button_Flag,
                subresp)[1] +nchar(Answ_Button_Flag), nchar(subresp))
      subresp<- get_num(subresp)
      
      # Was the answer correct?
      if(length(correctresp)> 0 & length(subresp)> 0){ # check if imput exists
        if(is.numeric(correctresp) & is.numeric(subresp)){ # check if input is valid
          if(subresp== correctresp){
            accuracy<- 1
          }else{
            accuracy<- 0
          }
        }else{ # write NAs if no accuracy data could be extracted
          subresp<- NA
          correctresp<- NA
          accuracy<- NA
          message("\nCan't calculate accuracy, invalid input!\n")
        }  
      }else{
        subresp<- NA
        correctresp<- NA
        accuracy<- NA
        
        message("\nCan't calculate accuracy, invalid input!\n")
      }
    
      
      # question start time:
      start<- get_num(unlist(strsplit(trialFile[1], ' '))[1])
      
      # question end time:
      end<- get_num(unlist(strsplit(trialFile[which(grepl(Answ_Button_Flag, trialFile))], " "))[1])
      
      # trial duration:
      
      duration_ms <- end -start
      
      # check for problems (to prevent crashing):
      if(length(subresp)==0){
        subresp<- NA
      }
      
      if(length(correctresp)==0){
        correctresp<- NA
      }
      
      if(length(start)==0){
        start<- NA
      }
      
      if(length(end)==0){
        end<- NA
      }
      
      if(length(duration_ms)==0){
        duration_ms<- NA
      }
      
      if(length(accuracy)==0){
        accuracy<- NA
      }
      
      # save in data frame:
      temp<- data.frame(sub, item, cond, seq, dependnum, start, end, duration_ms, correctresp,
                        subresp, accuracy)
      
      Quest<- rbind(Quest, temp)
      
      
      cat(toString(j)); cat(" ")
      
    }# end of j (item)
    
    
    if(summary){
      subQ<- subset(Quest, sub==i)
      Sub_acc[i]<- round(mean(subQ$accuracy)*100,2)
    }
    
    cat("\n")
  
  } # end of i (subject)
  
  if(summary){
    db<- data.frame(sub= seq(1, length(data), 1), meanAcc= Sub_acc)
    
    cat("\n"); cat("Average accuracy per subject:\n\n")
    print(db)
    cat("\n")
  }
  
  
  return(Quest)
  
} # end of fun

