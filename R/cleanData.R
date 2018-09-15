#' 
#' Clean up of pre-processed data
#' 
#' @author Martin R. Vasilev
#' 
#' @param data Dataframe containing the raw fixation data that were extracted with the
#' EMreading package
#' 
#' @param removeOutsideText A logical inicating whether to remove all fixations outside the text 
#' and/or screen area. The default is TRUE.
#' 
#' @param removeBlinks A logical indicating whether to exclude all blinks from the data. The 
#' default (and recommended) setting is TRUE.
#' 
#' @param removeOutliers A logical indicating whether to remove outliers from the data (default
#' is TRUE). This parameter also requires the specification of a method and cutoff for
#' outlier exclusion (see below)
#' 
#' @param outlierCutoff Specifies the exclusion of long fixations (outliers). If set to FALSE,
#' no outliers will be exluded. If set to a number, fixations greater than that number will be
#' excluded as outliers. Default is to exclude fixations longer than 800 ms as outliers.
#' 
 
cleanData<- function(data= raw_fix, removeOutsideText= TRUE, removeBlinks= TRUE, 
                     removeOutliers= TRUE, outlierMethod= "ms", outlierCutoff= 800,
                     removeSmallFix= TRUE, smallFixCutoff= 80, combineNearbySmallFix= TRUE){
  # check user input:
  if(!is.logical(removeOutliers)){
    stop("removeOutliers can only be set to TRUE or FALSE")
  }
  if(!is.element(toupper(outlierMethod), c("MS", "STD", "ST"))){
    stop("Only 'ms' and 'std' are allowed as outlier removal methods")
  }
  if(removeOutliers== TRUE & !is.numeric(outlierCutoff)){
    stop("Please set a cut-off number")
  }
  
  if(toupper(outlierMethod)== "MS" & outlierCutoff> 1200 | toupper(outlierMethod)== "MS" & outlierCutoff< 600){
    warning("This is unusual cutoff. Typical values are between 600-1200 ms")
  }
  
  if(toupper(outlierMethod)== "STD" & outlierCutoff> 3 | toupper(outlierMethod)== "STD" & outlierCutoff< 2.5){
    warning("This is unusual cutoff. Typical values are between 2.5-3 STD")
  }
  
  ### Print settings back to user:
  if(removeOutsideText){
    s1<- 'remove all fixations outside the text and screen area'
  }else{
    s1<- ''
  }
  
  if(removeBlinks){
    s2<- "remove all blinks from the data"
  }
  
  if(removeOutliers){
    s3<- paste("remove all fixation durations > ", outlierCutoff, " ",
               outlierMethod, " as outliers", sep= '')
  }else{
    s3<- ""
  }
  
  if(combineNearbySmallFix){
    s4<- paste("combine any small fixations < ", smallFixCutoff, 
               "ms within one character of another fixation", sep = '')
  }else{
    s4<- ''
  }
  
  if(removeSmallFix){
    s5<- paste("remove any remaining fixations < ", smallFixCutoff, "ms", sep= '')
  }else{
    s5<- ''
  }
  
  cat(paste("I will", s1, s2, s3, s4, s5, sep=', '))
  cat("\n")
  
  
    if(combineNearbySmallFix){
      which_comb<- NULL
      for(i in 1:nrow(raw_fix)){
        if(i>1){
          if(is.na(raw_fix$char_trial[i]) | is.na(raw_fix$char_trial[i-1]) | is.na(raw_fix$char_trial[i+1])){
            next;
          }
          
         if(raw_fix$fix_dur[i]< 80){
           prev<- abs(raw_fix$char_trial[i]- raw_fix$char_trial[i-1])
           after<- abs(raw_fix$char_trial[i]- raw_fix$char_trial[i+1])
           if(prev== 1){
             raw_fix$fix_dur[i-1]<- raw_fix$fix_dur[i-1] + raw_fix$fix_dur[i]
             which_comb<- c(which_comb, i)
             cat(paste("\nsub ", raw_fix$sub[i], ", trial ", raw_fix$item[i],
                   ":\n     fix ", raw_fix$fix_num[i], " (", raw_fix$fix_dur[i],
                   " ms)", " was merged with fix ", raw_fix$fix_num[i-1],
                   " (", raw_fix$fix_dur[i-1], " ms)",
                   ": new fix ", raw_fix$fix_num[i-1],
                   " (",  raw_fix$fix_dur[i]+ raw_fix$fix_dur[i-1], 
                   "ms)", sep=''))
             cat("\n")
            
           }
           if(after== 1){
             raw_fix$fix_dur[i+1]<- raw_fix$fix_dur[i+1] + raw_fix$fix_dur[i]
             which_comb<- c(which_comb, i)
             cat(paste("\nsub ", raw_fix$sub[i], ", trial ", raw_fix$item[i],
                       ":\n     fix ", raw_fix$fix_num[i], " (", raw_fix$fix_dur[i],
                       " ms)", " was merged with fix ", raw_fix$fix_num[i+1],
                       " (", raw_fix$fix_dur[i+1], " ms)",
                       ": new fix ", raw_fix$fix_num[i+1],
                       " (",  raw_fix$fix_dur[i]+ raw_fix$fix_dur[i+1], 
                       "ms)", sep='')) 
             cat("\n")
           }
           
         }
        }
      }
      
      if(length(which_comb)>0){
         raw_fix<- raw_fix[-which_comb,]
      }
      
    }
  
  
  nstart<- nrow(raw_fix)
  
  # remove fixations outside of bounds and text:
  if(removeOutsideText){
    raw_fix<- subset(raw_fix, outOfBnds==0 & outsideText==0)
    raw_fix$outOfBnds<- NULL
    raw_fix$outsideText<- NULL
  }
  nOutBnds<- nstart- nrow(raw_fix)
  
  # remove blinks:
  if(removeBlinks){
    raw_fix<- subset(raw_fix, blink== 0 & prev_blink==0)
    raw_fix$blink<- NULL
    raw_fix$prev_blink<- NULL
    raw_fix$after_blink<- NULL
  }
  nblink<- nstart- nOutBnds - nrow(raw_fix)
  
  
  if(removeOutliers){
    out<- which(raw_fix$fix_dur > outlierCutoff)
    o<- raw_fix[out,]
    raw_fix<- raw_fix[-out,]
  }
  nOutlier<- nstart- nOutBnds - nblink- nrow(raw_fix)
  
  output<- paste("\nRemoved fixations: \n", "- outside of text or screen area: ", 
                 round((nOutBnds/nstart)*100, 4), " % \n",
                 "- due to blinks: ", round((nblink/nstart)*100, 4), " % \n",
                 "- outliers: ", round((nOutlier/nstart)*100, 4), " % \n \n",
                 "Remaining fixations: ", round((nrow(raw_fix)/nstart)*100, 4),
                 " % \n", sep = '')
  
  cat(output)
  
  
  if(removeOutliers & nrow(o)>0 & length(unique(o$cond))>0){
    test<- chisq.test(table(o$cond))
    
    if(test$p.value<0.05){
      warning("\n Chi-square test detects enequal number of outliers excluded per condition! \n")
      cat(paste("X^2(", test$parameter, ")", "= ", round(test$statistic, 4),
                ", p= ", test$p.value, sep= ''))
      cat("\n")
      
      cat(table(o$cond))
    }
  }
  
  
  return(raw_fix)
  
  
}
   
