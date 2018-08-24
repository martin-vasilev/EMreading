
#' Main function for pre-processing fixations and vertical re-alignment
#'
#' This function reads in data from .asc files and performs the pre-processing.
#'
#' @author Martin R. Vasilev
#'
#' @param data_list Directory of a txt file that contains all the .asc data file names.
#' In the .txt file, each .asc data file should appear on a separate row, e.g.:
#' C:/My Data/subject1.asc.
#' C:/My Data/subject2.asc.
#' If you want to process only one .asc file, just supply the file location in the string, e.g.:
#' C:/My Data/subject1.asc.
#'
#' @param ResX X screen resolution in pixels
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
#' as an image file. The default is TRUE. If set to FALSE, no images will be plotted.
#' Note that plotting images will generally take longer time to pre-process the data.
#' The images are saved in the current working directory in the folder "img".
#'
#' @param RSpar A vector of parameter values (n=3) for detecting return sweeps in the data.
#' It's usually best not to change this unless you know what you are doing. The default values
#' have been tested to work well in most cases.
#' 1). Minimum Y-distance travelled as a function of line height (1= saccade has to travel
#' the height of one line of text)
#' 2). Minimum X-distance travelled in characters to the left
#' 3). How much the saccade went down compared to the y postion of the previous fixation.
#' This is again measured in line heights (1= the eye travelled 1 line height below the
#' previous fixation).
#' Use= c(1/4, 8, 2/3)
#'
#' @return A data frame containing the data
#'
#' @example
#' myData<- paraFix(data_list= "preproc/files.txt", ResX= 1920, ResY=1080, maxtrial= 120,
#' align=TRUE, RSpar= c(1/4, 8, 2/3), plot=TRUE)
#'
#' @include utility.R

paraFix<- function(data_list= "preproc/files.txt", ResX= 1920, ResY=1080, maxtrial= 120,
                   align=TRUE, RSpar= c(1/4, 8, 2/3), plot=TRUE, keepLastFix=TRUE){

  # check file input:
  if(grepl('.txt', data_list)){
    data<- readLines(data_list, warn=F) # process multiple files
  }else{
    data<- data_list # process only 1 file
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
        raw_fix_temp<- parse_fix(file, map, coords, trial_db[j,], i, ResX, ResY, keepLastFix)

        # Align fixations:
        if(align){
          RFalignTemp<- reAlign(raw_fix_temp, coords, map, ResX, ResY,
                                Ythresh= RSpar[1],Xthresh= RSpar[2],
                                threshSimilar= RSpar[3])
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

} # end of ParaFix
