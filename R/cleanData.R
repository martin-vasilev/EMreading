#' 
#' Clean up of pre-processed data
#' 
#' @author Martin R. Vasilev
#' 
#' @param raw_fix Raw fixations that were pre-processed with the EMreading package
#' 
 
cleanData<- function(raw_fix){
   
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
   
   cat(paste("Removed fixations: \n", "- outside of text or screen: ", 
             round((nOutBnds/nstart)*100, 4), " % \n",
             "- due to blinks: ", round((nblink/nstart)*100, 4), " % \n\n",
             "Remaining fixations: ", round((nrow(raw_fix)/nstart)*100, 4),
             sep = ''))
   
   return(raw_fix)

 }