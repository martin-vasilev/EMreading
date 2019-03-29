#' Pre-processing of data using DA1 files
#'
#' This function reads in data from .asc files and DA1 files (from EyeDoctor) and merges them together.
#'
#' @author Martin R. Vasilev
#'
#' @param data_dir Input of data files to be processed, as a directory that contains both the ASC
#' and the DA1 files for all subjects
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
#' Add example here
#' @include utility.R
#' 

preprocFromDA1<- function(data_dir= NULL, ResX= 1920, ResY=1080, maxtrial= 120, 
                          tBlink= 50, textStim= NULL, ppl= NULL, xOffset=NULL, plot=FALSE){
  
  options(scipen=999)
  
  # check if user provided data dir:
  if(length(data_dir)==0){
    data_dir= file.choose() # make them chose a file
    message("To process multiple files, please specify a directory in 'data_dir'")
  }
  
  
  # Get data file names:
  dataASC<- get_files(data_dir)
  dataDA1<- get_files(data_dir, file_ext = 'DA1')
  
  if(length(dataASC)!= length(dataDA1)){
    stop("Unequal number of .ASC and .DA1 files detected! Please resolve this before running the script again.")
  }
  
  raw_fix<- NULL;
  warnMismatch= 0
  
  for (i in 1:length(dataDA1)){ # for each subject..
    
    cat(sprintf("Processing subject %i", i)); cat("\n")
    cat(sprintf("Loading data %s ...", dataASC[i]));
    filename= dataASC[i] #strsplit(data[i], "\\")
    
    file<- readLines(dataASC[i]) # load asc file
    fileDA<- readLines(dataDA1[i]) # load da1 file
    
    cat(" Done"); cat("\n")
    trial_db<- trial_info(file, maxtrial) # extract info about trials to be processed
    cat("Trial... ")
    
    for(j in 1:length(fileDA)){ # for each item
      
      #################################
      # prepare stuff from da1 file:
      
      # Parse info from da1 file:
      string<- fileDA[j] # get da1 data for current trial
      da<-data.frame( do.call( rbind, strsplit( string, ' ' ) ) )
      
      # get basic characteristics:
      nfix<- as.numeric(as.character(da$X8))
      seq<- as.numeric(as.character(da$X1))
      cond<- as.numeric(as.character(da$X2))
      item<- as.numeric(as.character(da$X3))
      sub<- i
      
      da2<- da[,-c(1:8)] # subset part of da1 that contains fixation data
      
      ####
      # Recode da1 file for easier processing:
      count<- 1
      da3<- NULL
      
      for(k in 1:(length(da2)/4)){ # for each fixation
        
        temp<- da2[1, count:(count+3)] # extract data:
        colnames(temp)<- c("char", "line", "start", "end") # change colnames for rbind
        
        # Change from factor to num:
        temp$char<- as.numeric(as.character(temp$char))
        temp$line<- as.numeric(as.character(temp$line))
        temp$start<- as.numeric(as.character(temp$start))
        temp$end<- as.numeric(as.character(temp$end))
        
        # increment char and line number by 1 (EyeDoctor counts from 0):
        temp[1,1:2]<- temp[1,1:2]+1
        
        da3<- rbind(da3, temp)
        count= count+4
      }
      
      da1<- da3; rm(da, da2, da3)
      da1$fix_num<- 1:nrow(da1)
      
      ##########################################
      
      # get trial files as in MultuLine.R:
      # Extract EyeTrack trial text:
      # find position in asc trial info db:
      whichDB<- which(trial_db$cond== cond & trial_db$item== item)
      
      text<- get_text(file[trial_db$ID[whichDB]:trial_db$start[whichDB]])
      try(coords<- suppressWarnings(get_coord(text)))
      try(map<- coord_map(coords, x=ResX, y= ResY)) # map them to pixels on the screen
      
      # Extract raw fixations from data and map them to the text:
      try(raw_fix_temp<- parse_fix(file, map, coords, trial_db[j,], i, ResX, ResY, tBlink, SL= T))
      
      # Find when the time when the trial starts:
      # NB!: For some weird reason, EyeDoctor starts counting trial time from the
      # "GAZE TARGET ON" flag in the asc data
      fileTrial<- file[trial_db$ID[whichDB]:trial_db$end[whichDB]]
      trial_start_flag<- fileTrial[which(grepl('GAZE TARGET ON', fileTrial))]
      trial_start<- get_num(trial_start_flag)
      
      # mark which fixations are still there
      raw_fix_temp$time_since_start<- raw_fix_temp$SFIX-trial_start
  
      
      #################################################################################
      # Merge da1 data with raw fixations:
      
      # check for fix num mismatch & print warnings
      if(nrow(raw_fix_temp)!= nrow(da1)){
        message(sprintf("Warning! Detected different number of fixations from da1 file for subject %g, item %g !!! \n", i, j))
        
        if(!warnMismatch){
          message("Please avoid deleting or merging fixations in EyeDoctor!\n\n")
          warnMismatch= 1
        }
      }
      
      raw_fix_new<- NULL
      raw_fix_temp$wordID<- as.character(raw_fix_temp$wordID)
      
      for(l in 1:nrow(da1)){ # for each fixation to be merged:
        
        # locate fixation (row number) from raw asc data using start time:
        a<- which(raw_fix_temp$time_since_start== da1$start[l])
        
        if(length(a)==0){
          stop(sprintf("Critical error: da1 fixation not found in asc data: subject %g, item %g, fix %g, fix_dur %g, char %g, line %g",
                       i, j, l, da1$end[l]- da1$start[l], da1$char[l]-1, da1$line[l]-1))
        }
        
        temp_fix<- raw_fix_temp[a,]
        
        # NOTE: stuff that appears as -1 in da1 will appear as 0 here
        
        # recode some variables using new (& possibly changed) fixation location:
        temp_fix$fix_num<- da1$fix_num[l] # fix number is taken from da1 sequence
        temp_fix$line<- da1$line[l] # line is taken from da1!
        
        # char from EyeDoctor is relative to line start!
        temp_fix$char_line <- da1$char[l]
        
        loc<- which(coords$line == temp_fix$line & coords$line_char== temp_fix$char_line)
        
        if(length(loc)>0){
          # update fixation with info from coords:
          temp_fix$sent<- coords$sent[loc] # sentence number
          temp_fix$word<- coords$word[loc] # word number
          temp_fix$char_trial<- as.numeric(as.character(coords$char[loc]))+1 # +1 bc EyeDoctor counts from 0
          temp_fix$wordID<- coords$wordID[loc] # word identity
          temp_fix$land_pos<- coords$char_word[loc] # landing position char
          
        }else{
          # update as NAs:
          temp_fix$sent<- NA # sentence number
          temp_fix$word<- NA # word number
          temp_fix$char_trial<- NA # +1 bc EyeDoctor counts from 0
          temp_fix$wordID<- NA # word identity
          temp_fix$land_pos<- NA # landing position char
          
          # redo char & line from above:
          temp_fix$char_line<- NA # stays NA unless changed below:
          temp_fix$line<- NA # stays NA unless changed below:
          
          #########
          # line
          
          nlines<- unique(coords$line)
          
          for(m in 1:length(nlines)){ # find on which "line" it occured (if any)
            y<- subset(coords, line== nlines[m])
            minY<- min(y$y1)
            maxY<- max(y$y2)
            
            if(isInside2D(temp_fix$yPos, minY, maxY)){
              temp_fix$line<- nlines[m] # fixation is on line m (but outside text area)
            }
          }
          
          #########
          # x pos (char_line)
          if(!is.na(temp_fix$line)){ # no point in continuing if fix is not on a "line"
            
            if(isInside2D(temp_fix$xPos, 1, ResX)){ # make sure fix is inside screen area..
              x<- subset(coords, line== temp_fix$line)
              minX<- min(x$x1)
              maxX<- max(x$x2)
              ppl<- mean(x$x2- x$x1) # take mean pixel per letter (in case of proportional font..)
              maxChar<- max(x$line_char)
              
              if(temp_fix$xPos< minX){ # fix is before left text margin
                temp_fix$char_line<- 1- ceiling((minX- temp_fix$xPos)/ppl)
                # make char negative (to indicate it's before line start)
              }
              
              if(temp_fix$xPos> maxX){ # fix is to the right of the right text margin
                temp_fix$char_line<- maxChar + ceiling((temp_fix$xPos - maxX)/ppl)
              }
              
            } # end of "if inside screen"
            
          } # end of "if on a line"
        
          
        } # end of "if not on text area"
        
        
        # add max line_chars (for fixations to the right of the right text margin)
        c<- subset(coords, line== temp_fix$line)
        if(nrow(c)>0){
          temp_fix$max_char_line<- max(c$line_char)
        }else{
          temp_fix$max_char_line<- NA
        }
        
        
        # merge data from curr iteration:
        raw_fix_new<- rbind(raw_fix_new, temp_fix)
        
        
      } # end of l loop (da1)
      
      raw_fix<- rbind(raw_fix, raw_fix_new)
      
      cat(toString(j)); cat(" ")
    } # end of item
    
    cat("\n DONE \n \n");
  } # end of subject
  
  cat("\n \n All Done!");
}

