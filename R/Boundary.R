
#' Checks the display change implementation in Boundary experiments (Rayner, 1975)
#'
#' Checks the implementation of 1 boundary change during sentence reading. Checks the timing of the display change,
#' as well as whether the boundary was crossed in a forwards saccade.
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
#' @param maxtrial Maximum number of experimental trials in the experiment (default= 9999)
#' 
#' @param boundary_loc The name of the message flag in the data file for the pixel location of the boundary (if present)
#' 
#' @param start_flag start flag for the beginning of the display change
#' 
#' @param end_flag end flag indicating the completion of the display change
#' @param blink_window_ms Number of milliseconds before the display change starts
#'   and after it completes to check for nearby blinks (default 200).
#' @param fixed_boundary Optional fixed boundary pixel coordinate.
#' @param horizontal_boundary TRUE to analyse x-coordinate, FALSE for y-coordinate.
#' @return Data frame including blinkBefore, blinkDuring, blinkAfter, blinkAny,
#'   tBlinkBefore (ms since last blink ended), and tBlinkAfter (ms until next
#'   blink starts). A missing blink is FALSE if change timestamps are available.
#'   Blink timing is NA if no blink is recorded before/after the change.
#' 
#' @include utility.R


Boundary<- function(data_list= '', maxtrial= 999, boundary_loc= 'BOUNDARY', 
                    fixed_boundary= NULL, horizontal_boundary= TRUE,
                    start_flag= 'DISPLAY CHANGE STARTED', end_flag= 'DISPLAY CHANGE COMPLETED',
                    blink_window_ms= 200){
  
  
  if (length(blink_window_ms) != 1L || !is.numeric(blink_window_ms) ||
      is.na(blink_window_ms) || !is.finite(blink_window_ms) ||
      blink_window_ms < 0) {
    stop("blink_window_ms must be a single non-negative number of milliseconds")
  }

  # EyeLink EBLINK records: EBLINK [eye] [start] [end] [duration].
  # Keep the complete interval, not just the end timestamp.
  get_trial_blinks <- function(trialFile) {
    lines <- grep("^[[:space:]]*EBLINK[[:space:]]", trialFile, value = TRUE)
    if (!length(lines)) {
      return(data.frame(start = numeric(0), end = numeric(0)))
    }
    fields <- strsplit(trimws(lines), "[[:space:]]+")
    times <- t(vapply(fields, function(x) {
      if (length(x) < 5L) return(c(NA_real_, NA_real_))
      suppressWarnings(as.numeric(x[3:4]))
    }, numeric(2)))
    blinks <- data.frame(start = times[, 1], end = times[, 2])
    blinks[is.finite(blinks$start) & is.finite(blinks$end) &
             blinks$end >= blinks$start, , drop = FALSE]
  }

  # check if user provided data dir:
  if(length(data_list)==0){
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
  
  dat<- NULL
  
  for (i in 1:length(data)){ # for each subject..
    
    cat(sprintf("\nProcessing subject %i", i)); cat("\n")
    cat(sprintf("Loading data %s ...", data[i]));
    filename= data[i] #strsplit(data[i], "\\")
    
    file<- readLines(data[i]) # load file
    cat(" Done"); cat("\n")
    trial_db<- trial_info(file, maxtrial= maxtrial, selectEXP = T) # extract info about trials to be processed
    cat("Trial... ")
    
    for(j in 1:nrow(trial_db)){ # for each item
      
      trialFile<- file[trial_db$ID[j]:trial_db$end[j]]
      
      temp<- data.frame(sub=NA, item= NA, cond=NA, seq= NA, Bnd_loc= NA, tCross=NA, tStarted=NA, tCompleted=NA, 
                       tChange=NA, tActualChange= NA, prevFlag= NA, nextFlag= NA, tPrevFlag= NA, tNextFlag=NA,
                       tChangetoFixOnset= NA, blinkBefore= NA, blinkDuring= NA,
                       blinkAfter= NA, blinkAny= NA, tBlinkBefore= NA_real_,
                       tBlinkAfter= NA_real_)
      temp$prevFlag<- NULL
      temp$tPrevFlag<- NULL
      
      temp$sub<- i
      temp$item<- trial_db$item[j]
      temp$cond<- trial_db$cond[j]
      temp$seq<- j
      
      # Try to find boundary location:
      Bnd_txt<- trialFile[which(grepl(boundary_loc, trialFile))]
      loc<- substr(Bnd_txt[1], unlist(gregexpr(' ', Bnd_txt[1]))[1]+nchar(boundary_loc)+1, nchar(Bnd_txt[1]))
      #loc<- as.numeric(loc) 
      temp$Bnd_loc<- loc
      
      # overwrite if using a fixed boundary:
      if(!is.null(fixed_boundary)){
        temp$Bnd_loc<- fixed_boundary
      }
      
      # Find the start flag:
      Start_text<- trialFile[which(grepl(start_flag, trialFile))]
      start_time<-  substr(Start_text, 1, unlist(gregexpr(' ', Start_text))[1])
      start_time<- get_num(start_time)
      
      if(length(Start_text)==0){
        dat<- rbind(dat, temp)
        next
      }
      temp$tStarted<- start_time
      
      # Find the end flag:
      End_text<- trialFile[which(grepl(end_flag, trialFile))]
      if (length(End_text)==0) {
        dat<- rbind(dat, temp)
        next
      }
      end_time<-  substr(End_text, 1, unlist(gregexpr(' ', End_text))[1])
      end_time<- get_num(end_time)
      temp$tCompleted<- end_time
      
      # difference between started and completed:
      temp$tChange<- temp$tCompleted - temp$tStarted

      # Blink intervals relative to the display change.
      # Before: the blink ENDED within the preceding window.
      # During: a blink overlaps the display-change interval.
      # After: the blink STARTED within the following window.
      if (is.finite(temp$tStarted) && is.finite(temp$tCompleted)) {
        blinks <- get_trial_blinks(trialFile)
        preceding <- blinks$end[blinks$end < temp$tStarted]
        following <- blinks$start[blinks$start > temp$tCompleted]

        if (length(preceding)) {
          temp$tBlinkBefore <- temp$tStarted - max(preceding)
        }
        if (length(following)) {
          temp$tBlinkAfter <- min(following) - temp$tCompleted
        }

        temp$blinkBefore <- !is.na(temp$tBlinkBefore) &&
          temp$tBlinkBefore <= blink_window_ms
        temp$blinkDuring <- any(blinks$start <= temp$tCompleted &
                                 blinks$end >= temp$tStarted)
        temp$blinkAfter <- !is.na(temp$tBlinkAfter) &&
          temp$tBlinkAfter <= blink_window_ms
        temp$blinkAny <- temp$blinkBefore || temp$blinkDuring || temp$blinkAfter
      }
      
      # what is the actual time the boudary was crossed?
      whichRow<- which(grepl(start_flag, trialFile))[1]
      if (whichRow > 1L) {
        samples <- trialFile[seq.int(max(1L, whichRow - 51L), whichRow - 1L)]
      } else {
        samples <- character(0)
      }
      
      # Retain gaze samples only, excluding blink/event/message lines.
      samples <- samples[grepl("^[[:space:]]*[0-9]+[[:space:]]", samples)]
      pieces <- strsplit(trimws(samples), "[[:space:]]+")
      pieces <- pieces[vapply(pieces, length, integer(1)) >= 3L]
      time_cross <- integer(0)
      if (length(pieces)) {
        sample_mat <- do.call(rbind, lapply(pieces, function(x) x[1:3]))
        sample_time <- suppressWarnings(as.numeric(sample_mat[, 1]))
        sample_x <- suppressWarnings(as.numeric(sample_mat[, 2]))
        sample_y <- suppressWarnings(as.numeric(sample_mat[, 3]))
        bnd <- suppressWarnings(as.numeric(temp$Bnd_loc))

        if (isTRUE(horizontal_boundary)) {
          time_cross <- which(sample_x >= bnd)
        } else {
          time_cross <- which(sample_y >= bnd)
        }
      }

      if(length(time_cross)>0){
        temp$tCross<- sample_time[time_cross[1]]
      }else{
          temp$tCross<- temp$tStarted
      }
      
      # calculate actual change duration: 
      temp$tActualChange<- temp$tCompleted - temp$tCross
      
      AfterBnd= trialFile[whichRow:length(trialFile)]
      
      # Get next flag:
      nextSFIX<- which(grepl(c('SFIX') , AfterBnd))
      nextEFIX<- which(grepl(c('EFIX') , AfterBnd))
      
      if(!is.na(nextSFIX[1])& !is.na(nextEFIX[1])){
        
        if(nextSFIX[1]< nextEFIX[1]){
          temp$nextFlag<- 'SFIX'
          tNextFlag= AfterBnd[nextSFIX[1]]
          #tNextFlag= get_num(unlist(strsplit(tNextFlag, '\t'))[1])
          temp$tNextFlag<- get_num(tNextFlag)
        }else{
          temp$nextFlag<- 'EFIX'
          tNextFlag= AfterBnd[nextEFIX[1]]
          tNextFlag= get_num(unlist(strsplit(tNextFlag, '\t'))[1])
          temp$tNextFlag<- tNextFlag
        }
      }
      
      # timing of change relative to next fixation onset?
      
      if(!is.na(temp$nextFlag)){
        if(temp$nextFlag== "SFIX"){
          temp$tChangetoFixOnset<- temp$tCompleted - temp$tNextFlag
        }else{ # EFIX flag
          if (length(nextSFIX) >= 2L) {
            tNextSFIX= AfterBnd[nextSFIX[2]]
            tNextSFIX<- get_num(tNextSFIX)
            temp$tChangetoFixOnset<- tNextSFIX-temp$tCompleted
          }
        }
      }
      
      dat<- rbind(dat, temp)
      
      cat(toString(j)); cat(" ") 
      
    } # end of j
    
  } # end of i
  
  return(dat)
    
}