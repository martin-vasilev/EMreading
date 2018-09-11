
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
#' @return A data frame containing the data
#'
#' @example
#' myData<- SLpreproc(data_list= "preproc/files.txt", ResX= 1920, ResY=1080, maxtrial= 120,
#' plot=FALSE)
#'
#' @include utility.R

SLpreproc<- function(data_list= "preproc/files.txt", ResX= 1920, ResY=1080, maxtrial= 120, 
                     tBlink= 50, plot=FALSE, keepLastFix=TRUE){

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

  for (i in 1:length(data)){ # for each subject..

    cat(sprintf("Processing subject %i", i)); cat("\n")
    cat(sprintf("Loading data %s ...", data[i]));
    file<- readLines(data[i]) # load file
    cat(" Done"); cat("\n")
    trial_db<- trial_info(file, maxtrial) # extract info about trials to be processed
    cat("Trial... ")

    for(j in 1:nrow(trial_db)){ # for each item

      text<- get_text(file[trial_db$ID[j]:trial_db$start[j]]) # get text details (Eyetrack)

      if(text[1]!=0){ # if trial contained text
        coords<- get_coord(text) # extract text coordinates
        map<- coord_map(coords, x=ResX, y= ResY) # map them to pixels on the screen

        # Extract raw fixations from data and map them to the text:
        raw_fix_temp<- parse_fix(file, map, coords, trial_db[j,], i, ResX, ResY, tBlink,
                                 keepLastFix, SL= TRUE)

        # Align fixations:
        raw_fix<- rbind(raw_fix, raw_fix_temp)

        if(length(raw_fix_temp)>1 & plot==TRUE){ # if data was extracted from trial
          # create picture of fixations:
          plot_fix(coords, raw_fix_temp, i, j, ResX, ResY)
        }
      } else{ # if there was no text in trial, just extract fixations
        raw_fix_temp<- parse_fix(file, map=0, coords=0, trial_db[j,], i, ResX, ResY, keepLastFix, hasText=FALSE)
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
  raw_fix<- raw_fix[,-c(12,15)]
  
  return(raw_fix)

} # end of ParaFix
