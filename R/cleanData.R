#' 
#' Clean up of pre-processed data
#' 
#' @author Martin R. Vasilev
#' 
#' @param raw_fix Raw fixation data that were extracted with the EMreading package
#' 
#' @param outlierCutoff Specifies the exclusion of long fixations (outliers). If set to FALSE,
#' no outliers will be exluded. If set to a number, fixations greater than that number will be
#' excluded as outliers. Default is to exclude fixations longer than 800 ms as outliers.
#' 
 
cleanData<- function(raw_fix, outlierCutoff= 800){
   
   nstart<- nrow(raw_fix)
    
   # remove fixations outside of bounds and text:
   raw_fix<- subset(raw_fix, outOfBnds==0 & outsideText==0)
   nOutBnds<- nstart- nrow(raw_fix)
   raw_fix$outOfBnds<- NULL
   raw_fix$outsideText<- NULL
   
   # remove blinks:
   raw_fix<- subset(raw_fix, blink== 0 & prev_blink==0)
   nblink<- nstart- nOutBnds - nrow(raw_fix)
   raw_fix$blink<- NULL
   raw_fix$prev_blink<- NULL
   raw_fix$after_blink<- NULL
   
   if(outlierCutoff!= FALSE){
     raw_fix<- subset(raw_fix, fix_dur<= outlierCutoff)
     nOutlier<- nstart- nOutBnds - nblink- nrow(raw_fix)
   }
   
   output<- paste("Removed fixations: \n", "- outside of text or screen: ", 
                  round((nOutBnds/nstart)*100, 4), " % \n",
                  "- due to blinks: ", round((nblink/nstart)*100, 4), " % \n",
                  "- outliers: ", round((nOutlier/nstart)*100, 4), " % \n \n",
                  "Remaining fixations: ", round((nrow(raw_fix)/nstart)*100, 4),
                  sep = '')
   
   cat(output)
   
   return(raw_fix)

 }