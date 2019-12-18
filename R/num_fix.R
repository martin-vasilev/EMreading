
#' Calculation of number of fixations per trial using raw fixation data. 
#' 
#' 
#' Output includes: number of 1st-pass fixations, number of 2nd-pass fixations, and number of all fixations in trial.
#' Note: if you need number of fixations PER WORD, this is given by the 'wordMeasures' function.
#' 
#' @author Martin R. Vasilev
#' 
#' @param data Dataframe containing the raw fixation data that were extracted with the
#' EMreading package
#' 

num_fix<- function(data){
  
  new_data<- NULL
  
  nsubs<- unique(data$sub)
  
  cat(sprintf("\nProcessing subject "))
  
  for(i in 1:length(nsubs)){
    n<- subset(data, sub== nsubs[i])
    nitems<- unique(n$item)
    
    cat(toString(i)); cat(" ") 
    
    for (j in 1:length(nitems)){
      m<- subset(n, item== nitems[j])
      
      first<- subset(m, regress==0)
      second<- subset(m, regress==1)
      
      t<- m[1,]
      t$Nfix_1st <- nrow(first)
      t$Nfix_2nd<- nrow(second)
      t$Nfix_all<- nrow(m)
      
      new_data<- rbind(new_data, t)
    } # end of items j
  } # end of subjects i
  
  ## Get rid of some columns we don't need:
  new_data$SFIX<- NULL
  new_data$EFIX<- NULL
  new_data$xPos<- NULL
  new_data$yPos<- NULL
  new_data$fix_num<- NULL
  new_data$fix_dur<- NULL
  new_data$sacc_dur<- NULL
  new_data$sent<- NULL
  new_data$word<- NULL
  new_data$word_line<- NULL
  new_data$char_trial<- NULL
  new_data$regress<- NULL
  new_data$wordID<- NULL
  new_data$land_pos<- NULL
  new_data$sacc_len<- NULL
  new_data$blink<- NULL
  new_data$prev_blink<- NULL
  new_data$after_blink<- NULL
  new_data$outOfBnds<- NULL
  new_data$outsideText<- NULL
  new_data$hasText<- NULL
  
  return(new_data)
  
} # end of function