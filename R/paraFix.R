
#' Main function for pre-processing fixations and vertical re-alignment
#'
#' This function reads in data from .asc files and performs the pre-processing.
#' 
#' @author Martin R. Vasilev
#' 
#' @param data_list A txt file containing all the data file names and the 
#' directory for accessing them: e.g. C:/My Data/subject1.asc. Each data file
#' should appear on a separate row.
#' 
#' @param ResX X Screen resolution in pixels 
#' 
#' @param ResY Y screen resolution in pixels
#' 
#' @param maxtrial Maximum number of trials in the experiment
#' 
#' @param align A logical indicating whether to perform re-alignment of fixations.
#' The default is TRUE. If set to FALSE, the script will return a data frame with 
#' only the raw fixations extracted from the data.
#' 
#' @param plot A logical indicating whether to plot the raw and re-aligned fixations 
#' as an image. The default is TRUE. If set to FALSE, no images will ve plotted. 
#' Note that plotting images will generally take longer time to pre-process the data.
#' The images are save in the current directory under "img".    
#' 
#' @return A data frame containing the data
#' 
#' @example 
#' myData<- paraFix(data_list= "preproc/files.txt", ResX= 1920, ResY=1080, maxtrial= 120,align=TRUE, plot=TRUE)

paraFix<- function(data_list= "preproc/files.txt", ResX= 1920, ResY=1080, maxtrial= 120,
                   align=TRUE, plot=TRUE, keepLastFix=TRUE){
  
  # Load functions:
  source("utility.R")
  
  data<- readLines(data_list, warn=F)

  raw_fix<- NULL; RFalign<- NULL
  
  for (i in 1:length(data)){ # for each subject..
    #  i=1; # temporary
    
    cat(sprintf("Processing subject %i", i)); cat("\n")  
    cat(sprintf("Loading data %s ...", data[i])); 
    file<- readLines(data[i]) # load file
    cat(" Done"); cat("\n") 
    trial_db<- trial_info(file, maxtrial) # extract info about trials to be processed
    cat("Trial... ")
    
    for(j in 1:nrow(trial_db)){ # for each item
      
      text<- get_text(file[trial_db$ID[j]:trial_db$start[j]])
      
      if(text[1]!=0){ # if trial contained text
        coords<- get_coord(text)
        map<- coord_map(coords, x=ResX, y= ResY)
        raw_fix_temp<- parse_fix(file, map, coords, trial_db[j,], i, ResX, ResY, keepLastFix)
        
        
        # Align fixations:
        if(align){
          RFalignTemp<- reAlign(raw_fix_temp, coords, map, ResX, ResY)
          RFalign<- rbind(RFalign, RFalignTemp)
          # plot re-aligned fixations
          plot_fix(coords, RFalignTemp, i, j, ResX, ResY, reAligned=T)
          
        } else{
          raw_fix<- rbind(raw_fix, raw_fix_temp) 
        }
        
        
        if(length(raw_fix_temp)>1 & plot==TRUE){ # if data was extracted from trial  
          # create picture of fixations:
          plot_fix(coords, raw_fix_temp, i, j, ResX, ResY)
        } 
      } else{ # if there was no text in trial, just extract fixations
        raw_fix_temp<- parse_fix(file, map=0, coords=0, trial_db[j,], i, ResX, ResY, keepLastFix, hasText=FALSE)
        raw_fix<- rbind(raw_fix, raw_fix_temp) 
        # create picture of fixations:
        
        if(length(raw_fix_temp)>1 & plot==TRUE){ # if data was extracted from trial 
          plot_fix(coords, raw_fix_temp, i, j, ResX, ResY, hasText = FALSE)
        }
      }
      
      
      cat(toString(j)); cat(" ")
    } # end of item loop
    
    cat("\n DONE \n \n"); 
  } # end of subject loop
  
  
  cat("\n \n All Done!"); 
  
  if(align){
    return(RFalign)
  }else{
    return(raw_fix)
  }
  
  
}
