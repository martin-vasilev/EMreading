#' 
#' Clean up of pre-processed data
#' 
#' @author Martin R. Vasilev
#' 
#' @param raw_fix Dataframe containing the raw fixation data that were extracted with the
#' EMreading package
#' 
#' @param removeOutsideText A logical inicating whether to remove all fixations outside the text 
#' and/or screen area. The default is TRUE.
#' 
#' @param removeBlinks A logical indicating whether to exclude all blinks from the data. The 
#' default (and recommended) setting is TRUE.
#' 
#' @param combineNearbySmallFix A logical indicating whether to merge small fixations < 80 ms 
#' that are within 1 character space of another fixation (the small fixation will be merged with
#'  the nearby fixation). The default is TRUE.
#'  
#' @param removeSmallFix A logical indicating whether to remove any remaining fixations smaller
#' than a certain cut-off. The default is TRUE
#' 
#' @param smallFixCutoff A numerical value indiciating the cut-off for removing small 
#' fixations (the default is 80 ms). Any values smaller than this number will be removed.
#' 
#' @param removeOutliers A logical indicating whether to remove outliers from the data (default
#' is TRUE). This parameter also requires the specification of a method and cutoff for
#' outlier exclusion (see below)
#' 
#' @param outlierMethod A string indicating what type of outlier exclusion method to use.
#' Allowed are 2 methods: "ms" for exclusion of fixations based on a number in milliseconds;
#' or "std" for exlusion based on values that are a certain number of standard deviations above
#' the subject's mean.
#' 
#' @param outlierCutoff Numerical value that specifies the cut-off for removing outliers. If you 
#' chose "ms" in "outlierMethod", please specify a number in ms (e.g., 800). if you chose "std",
#' please enter a number in standard deviations above the mean (e.g., 3).
#' 
#' @param silent A logical indicating whether to print output regarding merged fixations. 
#' Set to FALSE if you don't want this output (default is TRUE).
 
cleanData<- function(raw_fix= data, removeOutsideText= TRUE, removeBlinks= TRUE, 
                     combineNearbySmallFix= TRUE, removeSmallFix= TRUE, smallFixCutoff= 80,
                     removeOutliers= TRUE, outlierMethod= "ms", outlierCutoff= 800,
                     silent= FALSE){
  
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
    warning("This is unusual cut-off. Typical values are between 2.5-3 STD")
  }
  
  
  if(removeSmallFix & smallFixCutoff<50 | removeSmallFix & smallFixCutoff>100){
    warning("Unusual cut-off for removing short fixations: typical values are between 50-100 ms")
  }
  
  ### Print settings back to user:
  if(removeOutsideText){
    s1<- 'removed all fixations outside the text or screen area'
  }else{
    s1<- ''
  }
  
  if(removeBlinks){
    s2<- "removed all blinks from the data"
  }
  
  if(removeOutliers){
    s3<- paste("removed all fixation durations > ", outlierCutoff, " ",
               outlierMethod, " as outliers", sep= '')
  }else{
    s3<- ""
  }
  
  if(combineNearbySmallFix){
    s4<- paste("combined any small fixations < ", smallFixCutoff, 
               " ms within one character of another fixation", sep = '')
  }else{
    s4<- ''
  }
  
  if(removeSmallFix){
    s5<- paste("removed any remaining fixations < ", smallFixCutoff, " ms", sep= '')
  }else{
    s5<- ''
  }
  
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
             which_comb<- c(which_comb, i)
             
             if(!silent){
               cat(paste("\nsub ", raw_fix$sub[i], ", trial ", raw_fix$item[i],
                         ":  fix ", raw_fix$fix_num[i], " (", raw_fix$fix_dur[i],
                         " ms)", " was merged with fix ", raw_fix$fix_num[i-1],
                         " (", raw_fix$fix_dur[i-1], " ms)",
                         " -> new fix ", raw_fix$fix_num[i-1],
                         " (",  raw_fix$fix_dur[i]+ raw_fix$fix_dur[i-1], 
                         "ms)", sep=''))
               cat("\n")
             }

             # merge only AFTER printing output!
             raw_fix$fix_dur[i-1]<- raw_fix$fix_dur[i-1] + raw_fix$fix_dur[i]
            
           }
           if(after== 1){
             which_comb<- c(which_comb, i)
             
             if(!silent){
               cat(paste("\nsub ", raw_fix$sub[i], ", trial ", raw_fix$item[i],
                         ":  fix ", raw_fix$fix_num[i], " (", raw_fix$fix_dur[i],
                         " ms)", " was merged with fix ", raw_fix$fix_num[i+1],
                         " (", raw_fix$fix_dur[i+1], " ms)",
                         " -> new fix ", raw_fix$fix_num[i+1],
                         " (",  raw_fix$fix_dur[i]+ raw_fix$fix_dur[i+1], 
                         "ms)", sep='')) 
               cat("\n")
             }

             raw_fix$fix_dur[i+1]<- raw_fix$fix_dur[i+1] + raw_fix$fix_dur[i]
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
  
  if(removeSmallFix){
    raw_fix<- subset(raw_fix, fix_dur>= smallFixCutoff)
  }
  nSmallFix<- nstart- nOutBnds - nblink- nOutlier- nrow(raw_fix)
  
  
  # summary of what was done:
  cat("\n\n")
  cat(paste("What I did:", s1, s2, s3, s4, s5, sep='\n  - '))
  
  
  output<- paste("\n\nRemoved fixations (Report): \n", "  - outside of text or screen area: ", 
                 round((nOutBnds/nstart)*100, 4), " % \n",
                 "  - due to blinks: ", round((nblink/nstart)*100, 4), " % \n",
                 "  - outliers: ", round((nOutlier/nstart)*100, 4), " % \n",
                 "  - Small fixations: ", round((nSmallFix/nstart)*100, 4), " % \n \n",
                 "Remaining fixations: ", round((nrow(raw_fix)/nstart)*100, 4),
                 " % \n", sep = '')
  
  cat(output)
  
  
  if(removeOutliers & nrow(o)>0 & length(unique(o$cond))>1){
    test<- suppressWarnings(chisq.test(table(o$cond)))
    
    if(test$p.value<0.05){
      cat("\n Chi-square test detects enequal number of outliers excluded per condition! \n")
      cat(paste("X^2(", test$parameter, ")", "= ", round(test$statistic, 4),
                ", p= ", test$p.value, sep= ''))
      cat("\n")
      
      cat(table(o$cond))
    }
  }
  
  
  # remove hasText if all trial= TRUE
  if(length(which(raw_fix$hasText==1))== nrow(raw_fix)){
    raw_fix$hasText<- NULL
  }
  
  return(raw_fix)
  
  
}
   
