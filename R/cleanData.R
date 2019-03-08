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
#' @param combineMethod Method for combining small fixations. Accepted values are "char" for 
#' combaining based on a distance in characters and "pix" for combining based on distance in
#' pixels. The default is "char".
#' 
#' @param combineDist Distance for combining small fixations. Small fixations will be combined
#' only if they occur within this distance of another fixation. Based on the input to 
#' 'combineMethod' above, enter a numeric value either in number of characters or in number of
#' pixels. The default is 1 (characters).
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
#' 
#' @param silent A logical indicating whether to print output regarding merged fixations. 
#' Set to FALSE if you don't want this output (default is TRUE).
 
cleanData<- function(raw_fix= data, removeOutsideText= TRUE, removeBlinks= TRUE,
                     combineNearbySmallFix= TRUE, combineMethod= "char", combineDist= 1,
                     removeSmallFix= TRUE, smallFixCutoff= 80, removeOutliers= TRUE,
                     outlierMethod= "ms", outlierCutoff= 800, silent= FALSE){
  
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
    warning("This is unusual cutoff. Typical values are between 600-1200 ms\n")
  }
  
  if(toupper(outlierMethod)== "STD" & outlierCutoff> 3 | toupper(outlierMethod)== "STD" & outlierCutoff< 2.5){
    warning("This is unusual cut-off. Typical values are between 2.5-3 STD\n")
  }
  
  
  if(removeSmallFix & smallFixCutoff<50 | removeSmallFix & smallFixCutoff>100){
    warning("Unusual cut-off for removing short fixations: typical values are between 50-100 ms\n")
  }
  
  if(!is.element(combineMethod, c("char", "pix"))){
    stop("Invalid input for 'combineMethod': allowed values are 'char' for combining based on
         characters and 'pix' for pixel combning!")
  }
  
  if(combineMethod== "pix" & combineDist>20){
    warning("Unusually large pixel value for combining small fixations: typical values are 
            between 10-18 pixels (equivalent to 1 letter)\n")
  }
  
  if(combineMethod== "char" & combineDist>1){
    warning("Please note that the accepted distance for combining small fixations is 1 character\n")
  }
  

  
  #-------------------------------------------------------------------------------------------#
  
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
    if(outlierMethod=="std"){
      s3<- paste("removed all fixation durations > ", outlierCutoff, " ",
                 outlierMethod, " above the subject's mean as outliers", sep= '')
    }
    
  }else{
    s3<- ""
  }
  
  if(combineNearbySmallFix){
    if(combineMethod== "char"){
      method= " character(s)"
    }
    
    if(combineMethod== "pix"){
      method=  " pixels"
    }
    
    s4<- paste("combined any small fixations < ", smallFixCutoff, 
                 " ms within ", combineDist, method, 
                 " of another fixation", sep = '')
  }else{
    s4<- ''
  }
  
  if(removeSmallFix){
    s5<- paste("removed any remaining fixations < ", smallFixCutoff, " ms", sep= '')
  }else{
    s5<- ''
  }
  
    if(combineNearbySmallFix){
      
      if(silent){cat("Merging small fixations in the data ...")}
      
      nbefore<-nrow(raw_fix)
      which_comb<- NULL
      
      for(i in 1:nrow(raw_fix)){
        
  #      if(silent & is.element(i, unname(round(quantile(1:nrow(raw_fix))-1)[2:4]))){
  #        cat(".")}
        
        if(i>1 & i< nrow(raw_fix)){ # to prevent crashing on first/last row
          
          if(combineMethod== "char"){ # check for valid character value:
            if(is.na(raw_fix$char_trial[i]) | is.na(raw_fix$char_trial[i-1]) | is.na(raw_fix$char_trial[i+1])){
              next;
            }
          }

         if(raw_fix$fix_dur[i]< smallFixCutoff){
           if(combineMethod== "char"){
             prev<- abs(raw_fix$char_trial[i]- raw_fix$char_trial[i-1])
             after<- abs(raw_fix$char_trial[i]- raw_fix$char_trial[i+1])
           } else{
             prev<- abs(round(raw_fix$xPos[i])- round(raw_fix$xPos[i-1]))
             after<- abs(round(raw_fix$xPos[i])- round(raw_fix$xPos[i+1]))
           }
   
           if(prev<= combineDist){
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
           if(after<= combineDist){
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
  
  
  cat("\n ---------------------------------------------------------------\n")
  cat(paste("       Total merged fixations: ", length(which_comb), " (",
            round(100*(1-(nrow(raw_fix)/nbefore)),5), " % of total)", sep= ''))
  cat("\n ---------------------------------------------------------------\n")
  
  nstart<- nrow(raw_fix)
  
  # remove fixations outside of bounds and text:
  if(removeOutsideText){
   if(length(which(raw_fix$hasText==0))!= nrow(raw_fix)){
       raw_fix<- subset(raw_fix, outOfBnds==0 & outsideText==0 & hasText==1)
       raw_fix$outOfBnds<- NULL
       raw_fix$outsideText<- NULL
    }
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
    if(outlierMethod== "ms"){
      out<- which(raw_fix$fix_dur > outlierCutoff)
      o<- raw_fix[out,]
      raw_fix<- raw_fix[-out,] 
    }
    
    if(outlierMethod== "std"){
      nSubCutoff<- NULL
      out<- NULL
      outT<- NULL
      
      for(i in 1:length(unique(raw_fix$sub))){
        
        subM<- mean(raw_fix$fix_dur[raw_fix$sub==i]) # subject mean
        subSTD<- sd(raw_fix$fix_dur[raw_fix$sub==i]) # subject std
        cuttoff<- subM+ outlierCutoff*subSTD # cut-off (in STDs above mean)
        nSubCutoff[i]<- length(which(raw_fix$sub==i & raw_fix$fix_dur> cuttoff))
        
        # remove outlier fixations for subject:
        outT<- which(raw_fix$sub==i & raw_fix$fix_dur> cuttoff)
        out<- c(out, outT)
      }
      o<- raw_fix[out,]
      raw_fix<- raw_fix[-out,] 
    }
  }
  nOutlier<- nstart- nOutBnds - nblink- nrow(raw_fix)
  
  if(removeSmallFix){
    raw_fix<- subset(raw_fix, fix_dur>= smallFixCutoff)
  }
  nSmallFix<- nstart- nOutBnds - nblink- nOutlier- nrow(raw_fix)
  
  # if outlier removal was done by thhe std method:
  # print excluded outliers per sub
  if(outlierMethod== "std"){
    cat("\n\n Percentage distribution of excluded outlier fixations per subject:\n\n")
    nperc<- round((nSubCutoff/ sum(nSubCutoff))*100,2)
    df<- data.frame(1:length(nperc), nperc)
    colnames(df)<- c("Subject", "% total")
    print(df, row.names = FALSE)
    cat("      ----------\n")
    cat("            100 %")
  }
  
  
  # summary of what was done:
  cat("\n\n")
  cat(paste("What I did:", s1, s2, s3, s4, s5, sep='\n  - '))
  
  
  output<- paste("\n\nRemoved fixations (Report): \n", "  - outside of text or screen area: ", 
                 round((nOutBnds/nstart)*100, 3), " % \n",
                 "  - due to blinks: ", round((nblink/nstart)*100, 3), " % \n",
                 "  - outliers: ", round((nOutlier/nstart)*100, 3), " % \n",
                 "  - Small fixations (not combined): ",
                 round((nSmallFix/nstart)*100, 3), " % \n \n",
                 "Remaining fixations: ", round((nrow(raw_fix)/nstart)*100, 3),
                 " % \n", sep = '')
  
  cat(output)
  
  
  if(removeOutliers & nrow(o)>0 & length(unique(o$cond))>1){
    test<- suppressWarnings(chisq.test(table(o$cond)))
    
    if(test$p.value<0.05){
      cat("\n WARNING!!! Chi-square test detects enequal number of outliers excluded per condition! \n")
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
   