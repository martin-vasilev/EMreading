
#' Pre-processing of single-line reading data
#'
#' This function reads in data from .asc files and performs the pre-processing.
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
#' @param ResX X screen resolution in pixels
#'
#' @param ResY Y screen resolution in pixels
#'
#' @param maxtrial Maximum number of trials in the experiment
#' 
#' @param tBlink Time in milliseconds for detecting blinks before or after fixation. 
#' If there is a blink x milliseconds before or after the fixation, it will be marked
#' as having a blink. The default is 50 ms.
#'
#' @param plot A logical indicating whether to plot the raw and re-aligned fixations
#' as an image file. The default is TRUE. If set to FALSE, no images will be plotted.
#' Note that plotting images will generally take longer time to pre-process the data.
#' The images are saved in the current working directory in the folder "img".
#' 
#' @param textStim An optional parameter for cases when the text stimuli are not printed
#' to the .asc files. If this is the case, please provide a directory to a txt file 
#' containing the stimuli sentences used in the experiment. Each sentences should be
#' placed on a new line and the stimuli should be ordered in the way that they were presented
#' in the experiment (i.e., sentence 1 should be on line 1 and so on). You will also need
#' to provide information about the width of letters and the offset of the text on the 
#' x-axis (see below). The default is NULL for cases when the text stimuli are printed 
#' to the .asc files.
#' 
#' @param ppl Pixels per letter in the experiment (i.e., the width of each letter on the 
#' screen). Please note that this function currently works only with fixed-width (i.e., 
#' monospaced) fonts. Not needed if the text stimuli were printed to the data.
#' 
#' @param xOffset Offset of the text in pixels on the x-axis of the screen. This should be
#' the pixel location where the first letter of the sentence starts. Not needed if the text
#' stimuli were printed to the data.
#'
#' @return A data frame containing the data
#'
#' @example
#' myData<- SLpreproc(data_list= "preproc/files.txt", ResX= 1920, ResY=1080, maxtrial= 120,
#' plot=FALSE)
#'
#' @include utility.R

SLpreproc<- function(data_list= NULL, ResX= 1920, ResY=1080, maxtrial= 120, 
                     tBlink= 50, textStim= NULL, ppl= NULL, xOffset=NULL, plot=FALSE){
  
  options(scipen=999)
  
  # check if user provided data dir:
  if(!hasArg(data_list)){
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

  raw_fix<- NULL; RFalign<- NULL
  
  if(hasArg(textStim)){
     message("\n\nPlease always check the stimuli dimensions to make sure they are correct!\n")
     message("The stimuli mapping was developed with English stimuli, although it should
             in theory also work with other (spaced) alphabetical languages.\n")
     message("\nIf you notice any errors or inconsistencies, please let the developer know.\n")

     textFile<- readLines(textStim)
  }
  

  for (i in 1:length(data)){ # for each subject..

    cat(sprintf("Processing subject %i", i)); cat("\n")
    cat(sprintf("Loading data %s ...", data[i]));
    filename= data[i] #strsplit(data[i], "\\")
    #filename= filename[length(filename)]
    
    file<- readLines(data[i]) # load file
    cat(" Done"); cat("\n")
    trial_db<- trial_info(file, maxtrial) # extract info about trials to be processed
    cat("Trial... ")

    for(j in 1:nrow(trial_db)){ # for each item

      text<- get_text(file[trial_db$ID[j]:trial_db$start[j]]) # get text details (Eyetrack)

      if(text[1]!=0 | hasArg(textStim)){ # if trial contained text
        if(hasArg(textStim)){ # stims not printed to data
          try(coords<- mapTextSL(textFile[trial_db$item[j]], ppl, xOffset, ResY))
        }else{ # stims were printed to data
          try(coords<- get_coord(text)) # extract text coordinates
        }
        
        map<- coord_map(coords, x=ResX, y= ResY) # map them to pixels on the screen

        # Extract raw fixations from data and map them to the text:
        try(raw_fix_temp<- parse_fix(file, map, coords, trial_db[j,], i, ResX, ResY, tBlink, SL= TRUE))

        # Combine fixations:
        if(is.null(raw_fix_temp)){
          next;
        }

        raw_fix_temp$dataFile= filename
        raw_fix<- rbind(raw_fix, raw_fix_temp)

        if(length(raw_fix_temp)>1 & plot==TRUE){ # if data was extracted from trial
          # create picture of fixations:
          plot_fix(coords, raw_fix_temp, i, j, ResX, ResY)
        }
      } else{ # if there was no text in trial, just extract fixations
        try(raw_fix_temp<- parse_fix(file, map=0, coords=0, trial_db[j,], i, ResX, ResY, tBlink, hasText=FALSE, SL= TRUE))
        if(is.null(raw_fix_temp)){
          next;
        }
        raw_fix_temp$dataFile= filename
        raw_fix_temp$sub<- i
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
  
  # remove columns that are not useful for single line data:
  raw_fix$line<- NULL
  raw_fix$char_line<- NULL
  #raw_fix<- raw_fix[,-c(12,15)]
  
  return(raw_fix)

} # end of ParaFix
