#' Pre-processing of data using DA1 files
#'
#' This function reads in data from .asc files and DA1 files (from EyeDoctor) and merges them together.
#' It makes it possible to re-adjust the raw data for the vertical position of fixations using EyeDoctor's
#' DA1 data after manual processing.
#'
#' @author Martin R. Vasilev
#'
#' @param data_dir Input of data files to be processed, as a directory that contains BOTH the ASC
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
#' @param padding Padding amount used around the text margin (used to assign fixations close to the margin to a line). Default is 0. 
#' If padding is greater than 0, then all letter positions will be decremented by the specified number.
#' 
#' @param addNonFixatedWords A logical indicating whether to add non-fixated words to the dataset (default= TRUE).
#' Useful for skipping analyses, etc.
#' 
#' @param CrashOnMissingFix A logical indicating whether to stop script if a DA1 fixation is not found in ASC data file. 
#' This can occur if you let EyeDoctor merge fixations. 
#' 
#' @param DontMapBlinks A logical indicating whether to map fixation type (progressive, regressive) if the there is a 
#' blink on the current fixation (or immediately before or after it, as determined by tBlink; see above). If set to TRUE (default),
#'  fixations types are not mapped.
#'  
#' @param binocular A logical indicating whether the data is binocular (TRUE) or not (FALSE). False by default (assuming monocular data).
#' 
#' @return A data frame containing the data
#'
#' @example
#' Add example here
#' @include utility.R
#' 

preprocFromDA1<- function(data_dir= NULL, ResX= 1920, ResY=1080, maxtrial= 999, 
                          tBlink= 150, padding= 0, addNonFixatedWords= TRUE, CrashOnMissingFix= FALSE, DontMapBlinks=TRUE,
                          binocular=F){
  
  message(paste("Using", toString(padding), "letter(s) padding in the analysis!"))
  
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
      
      da1<- da[,-c(1:8)] # subset part of da1 that contains fixation data
      
      ####
      # Recode da1 file for easier processing:
      da1= as.data.frame(matrix(as.numeric(as.vector(t(da1))), ncol = 4,  byrow = TRUE),
                       stringsAsFactors = FALSE)
      colnames(da1)<- c("char", "line", "start", "end")
      da1$fix_num<- 1:nrow(da1)
      # increment char and line number by 1 (EyeDoctor counts from 0):
      da1$char<- da1$char+1
      da1$line<- da1$line+1
      
      ##########################################
      
      # get trial files as in MultuLine.R:
      # Extract EyeTrack trial text:
      # find position in asc trial info db:
      whichDB<- which(trial_db$cond== cond & trial_db$item== item)
      
      if(length(whichDB)==0){ # trial not present in asc file..
        next
      }
      
      text<- get_text(file[trial_db$ID[whichDB]:trial_db$start[whichDB]])
      try(coords<- suppressWarnings(get_coord(text, padding=0)))
      try(map<- coord_map(coords, x=ResX, y= ResY)) # map them to pixels on the screen
      
      # Extract raw fixations from data and map them to the text:
      try(raw_fix_temp<- parse_fix(file, map, coords, trial_db[whichDB,], i, ResX, ResY, tBlink, SL= F, binocular= binocular))
      
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
        message(sprintf("Warning! Detected different number of fixations between the asc (%g) and da1 (%g) file for subject %g, item %g !!! \n",
                        nrow(raw_fix_temp), nrow(da1), i, j))
        
        if(!warnMismatch){
          message("Please don't delete or merge fixations in EyeDoctor!\n\n")
          warnMismatch= 1
        }
      }
      
      raw_fix_new<- NULL
      raw_fix_temp$wordID<- as.character(raw_fix_temp$wordID)
      
      for(l in 1:nrow(da1)){ # for each fixation to be merged:
        
        # locate fixation (row number) from raw asc data using start time:
        a<- which(raw_fix_temp$time_since_start== da1$start[l])
        
        if(length(a)==0){
          if(CrashOnMissingFix){
            stop(sprintf("Critical error: da1 fixation not found in asc data: subject %g, item %g, fix %g, fix_dur %g, char %g, line %g",
                         i, j, l, da1$end[l]- da1$start[l], da1$char[l]-1, da1$line[l]-1)) 
          }else{
            warning(sprintf("Critical error: da1 fixation not found in asc data: subject %g, item %g, fix %g, fix_dur %g, char %g, line %g",
                            i, j, l, da1$end[l]- da1$start[l], da1$char[l]-1, da1$line[l]-1))
            next
          }
        }
        
        temp_fix<- raw_fix_temp[a,]
        
        # NOTE: stuff that appears as -1 in da1 (outside of text) will appear as 0 here
        
        # recode some variables using new (& possibly changed) fixation location:
        temp_fix$fix_num<- da1$fix_num[l] # fix number is taken from da1 sequence
        temp_fix$line<- da1$line[l] # line is taken from da1!
        
        # char from EyeDoctor is relative to line start! (we also remove padding, if present)
        temp_fix$char_line <- da1$char[l]- padding
        
        loc<- which(coords$line == temp_fix$line & coords$line_char== temp_fix$char_line)
        loc_orig<- which(coords$line == raw_fix_temp$line[a] & coords$line_char== raw_fix_temp$char_line[a])
        
        # save whether current fixation was remapped (for statistics):
        temp_fix$reMapped<- NA
        if(length(loc)>0 & length(loc_orig)>0){
          if(!is.null(loc)){
            if(!is.null(loc_orig)){
              try(temp_fix$reMapped<- ifelse(loc==loc_orig, 0, 1))
            }
          }
        }
        
        
        if(length(loc)>0){
          
          # update fixation with info from coords:
          temp_fix$sent<- coords$sent[loc] # sentence number
          temp_fix$word<- coords$word[loc] # word number
          temp_fix$word_line<- coords$word_line[loc] # word number on line
          temp_fix$char_trial<- as.numeric(as.character(coords$char[loc]))+1 # +1 bc EyeDoctor counts from 0
          temp_fix$wordID<- coords$wordID[loc] # word identity
          temp_fix$land_pos<- coords$char_word[loc] # landing position char
          temp_fix$outsideText<- 0
          
          #if(da1$char)
          
        }else{
          # update as NAs:
          temp_fix$sent<- NA # sentence number
          temp_fix$word<- NA # word number
          temp_fix$word_line<- NA # word number
          temp_fix$char_trial<- NA # +1 bc EyeDoctor counts from 0
          temp_fix$wordID<- NA # word identity
          temp_fix$land_pos<- NA # landing position char
          temp_fix$outsideText<- 1
          
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
           }# end of m
          
          
          # the m-loop above does not take into account whether the fix line was manually adjusted in EyeDoctor.
          # To fix this, we check if the EyeDoctor line corresponds to the line assigned above. If it doesn't, 
          # we default to Eyedoctor coding (since we assume that's what the user wants).
          
          if(!is.na(da1$line[l])){ # continue only if line value isn't missing..
            if(!is.na(temp_fix$line)){
              if(da1$line[l]!= temp_fix$line){ # if there is line mismatch...
                temp_fix$line<- da1$line[l] # use EyeDoctor line
              } 
            }
          }
          
          
          # 
          #########
          # x pos (char_line)
          # if(!is.na(temp_fix$line)){ # no point in continuing if fix is not on a "line"
          #   
          #   if(isInside2D(temp_fix$xPos, 1, ResX)){ # make sure fix is inside screen area..
          #     x<- subset(coords, line== temp_fix$line)
          #     minX<- min(x$x1)
          #     maxX<- max(x$x2)
          #     ppl<- mean(x$x2- x$x1) # take mean pixel per letter (in case of proportional font..)
          #     maxChar<- max(x$line_char)
          #     
          #     if(temp_fix$xPos< minX){ # fix is before left text margin
          #       temp_fix$char_line<- 1- ceiling((minX- temp_fix$xPos)/ppl)
          #       # make char negative (to indicate it's before line start)
          #     }
          #     
          #     if(temp_fix$xPos> maxX){ # fix is to the right of the right text margin
          #       temp_fix$char_line<- maxChar + ceiling((temp_fix$xPos - maxX)/ppl)
          #     }
          #     
          #   } # end of "if inside screen"
          #   
          # } # end of "if on a line"
        
          
        } # end of "if not on text area"
        
        
        # add max line_chars (for fixations to the right of the right text margin)
        c<- subset(coords, line== temp_fix$line)
        if(nrow(c)>0){
          temp_fix$max_char_line<- max(c$line_char)
        }else{
          temp_fix$max_char_line<- NA
        }
        
        if(!is.na(temp_fix$char_line)){ # outside text if fix falls in the right margin padded area
          if(temp_fix$char_line> temp_fix$max_char_line){
            temp_fix$outsideText<- 1
          } 
        }
        
        # merge data from curr iteration:
        raw_fix_new<- rbind(raw_fix_new, temp_fix)
        
      } # end of l loop (da1)
      
      
      ## remap other variables (saccade length, regress prob.) & add new ones:
      
      # max word for each sentence:
      curr_sent<- matrix(0, max(coords$sent),4) # 2 col= max word; 3 col= max word in sent; 4 col= sent pass complete
      curr_sent[,1]<- c(1:max(coords$sent))
      
      # max words in current sentence:
      curr_sent[,3]<- c(coords$word[which(diff(coords$sent)==1)], coords$word[nrow(coords)])
      
      
      # reset prev values of variables:
      raw_fix_new$sacc_len<- NA
      raw_fix_new$regress<- NA
      raw_fix_new$regress2nd<- NA
      
      raw_fix_new$Rtn_sweep<- NA
      raw_fix_new$Rtn_sweep_type<- NA
      
      currentSent= 1
      currentLine= 1
      maxLine= 1
      newSent= FALSE
      inReg= FALSE
      
      same_line= NA
      
      for(m in 1:nrow(raw_fix_new)){
        
        # saccade length stuff:
        if(m>1){ # there is no sacc len on first fix, so we start at m>1
          if(!is.na(raw_fix_new$char_line[m-1]) & !is.na(raw_fix_new$char_line[m])){
            raw_fix_new$sacc_len[m]<- abs(raw_fix_new$char_line[m]- raw_fix_new$char_line[m-1])
          }
          
        }
        
        
        ######################################
        
        # Add return sweep stuff..
        if(!is.na(raw_fix_new$line[m])){
          currentLine<- raw_fix_new$line[m]
        }
        
        
        if(currentLine> maxLine){
          maxLine<- currentLine # update max line
          raw_fix_new$Rtn_sweep[m]<- 1 # return sweep
          
          # what type of return sweep is it?
          if(m<nrow(raw_fix_new)){
            
            # check if fixations are on the same line:
            if(!is.na(raw_fix_new$line[m+1])){
              if(!is.na(raw_fix_new$line[m])){
                if(raw_fix_new$line[m+1]== raw_fix_new$line[m]){
                  same_line= TRUE
                }else{
                  same_line= FALSE
                }
              }else{
                same_line= TRUE
              }
            }else{
              same_line= TRUE
            }
            
            if(raw_fix_new$xPos[m+1]< raw_fix_new$xPos[m]){ # leftward saccade on next fix
              #if(same_line){
              raw_fix_new$Rtn_sweep_type[m]<- "undersweep"
              #}else{
              #  raw_fix_new$Rtn_sweep_type[m]<- "accurate/line-final"
              #}
              
            }else{
              #if(same_line){
              raw_fix_new$Rtn_sweep_type[m]<- "accurate"
              #}else{
              #  raw_fix_new$Rtn_sweep_type[m]<- "accurate/line-final"
              #}
              
            }
            
          }else{
            raw_fix_new$Rtn_sweep_type[m]<- NA
          }
          
        }else{
          raw_fix_new$Rtn_sweep[m]<- 0
        }
        
        ##################################
        
        if(DontMapBlinks){
          # skip row if there is a blink:
          
          # current:
          if(!is.na(raw_fix_new$blink[m])){
            if(raw_fix_new$blink[m]==1){
              next
            }
          }
          
          # # previous:
          # if(!is.na(raw_fix_new$prev_blink[m])){
          #   if(raw_fix_new$prev_blink[m]==1){
          #     next
          #   }
          # }
          # 
          # # next:
          # if(!is.na(raw_fix_new$after_blink[m])){
          #   if(raw_fix_new$after_blink[m]==1){
          #     next
          #   }
          # }
          
        }
        
       # regression stuff
        if(!is.na(raw_fix_new$sent[m])){
          
          if(m==1){
            curr_sent[raw_fix_new$sent[m], 2]<- raw_fix_new$word[m] # first fixated word is current max word
            raw_fix_new$regress[m]<- 0 # first fix can never be a regression
            
          }else{
            
            if(raw_fix_new$word[m]> curr_sent[raw_fix_new$sent[m], 2]){
              curr_sent[raw_fix_new$sent[m], 2]<- raw_fix_new$word[m] #update new max word
              inReg<- FALSE
            }
            
            # did we finish current sentence?
            #maxWord<- curr_sent[raw_fix_new$sent[m], 2]== curr_sent[raw_fix_new$sent[m], 3]
            #newSent<- currentSent< raw_fix_new$sent[m] 
            if(currentSent< raw_fix_new$sent[m]){
              curr_sent[currentSent, 4]<- 1 # sentence 1st-pass complete
              currentSent<- raw_fix_new$sent[m] 
            }
            
            # if new sentence, terminate first pass of previous sentence:
            if(!is.na(raw_fix_new$sent[m-1])){ # continue only if previous sent is not NA..
              if(raw_fix_new$sent[m]> raw_fix_new$sent[m-1]){
                curr_sent[raw_fix_new$sent[m-1], 4]<- 1
              }
            }
            
            if(raw_fix_new$word[m]< curr_sent[raw_fix_new$sent[m], 2] & curr_sent[raw_fix_new$sent[m], 4]==0){
              raw_fix_new$regress[m]<- 1 # regression
              inReg<- TRUE
            }else{
              
              if(curr_sent[raw_fix_new$sent[m], 4]==0){
                raw_fix_new$regress[m]<- 0 # no regression
                
                if(raw_fix_new$word[m]== curr_sent[raw_fix_new$sent[m], 2] & inReg){
                  raw_fix_new$regress[m]<- 1 # still regressive
                }
                
              }else{
                raw_fix_new$regress[m]<- 1 # regression (2nd pass one)
                raw_fix_new$regress2nd[m]<- 1
                inReg<- TRUE
              }
              
              
              
              # if(!is.na(raw_fix_new$regress[m-1])){
              #   # if prev fix was regress and curr_word== max_word, that's still re-reading fixation
              #   
              #   if(raw_fix_new$regress[m-1]==1 & raw_fix_new$word[m]<= curr_sent[raw_fix_new$sent[m], 2]){
              #     raw_fix_new$regress[m]<- 1 # still regression
              #   }
              #   
              #}
                
              
              
            }
            
          }
          
        } # end of regression stuff
        
        
     # st<- raw_fix_new[,c("fix_num", "sent", 'line', 'word', 'wordID', 'regress', 'regress2nd')]
      
        
      } # end of m
      
      
      # Add non-fixated words in each sentence:
      if(addNonFixatedWords){
        
        for(s in 1:max(coords$sent)){
          
          t<- subset(raw_fix_new,  sent== s) # fixations that happened on this sentence
          sent_w<- subset(coords, sent== s) # all words in current sentence
          unique_read<- unique(t$word) # all word in curr sentence that were fixated
          unique_sent<- unique(sent_w$word) # all words in sentence that were presented
          not_fixated<- setdiff(unique_sent, unique_read)
          
          sent_1<- subset(sent_w, char_word==1)
          
          if(length(not_fixated)>0){ # add info about words
            temp<- raw_fix_new[1,]
            temp[1,]<- NA
            
            temp <- as.data.frame(lapply(temp, rep, length(not_fixated)))
            temp$sub<- raw_fix_new$sub[1]
            temp$item<- raw_fix_new$item[1]
            temp$cond<- raw_fix_new$cond[1]
            temp$seq<- raw_fix_new$seq[1]
            temp$sent<- s
            temp$word<- not_fixated
            temp$line<- sent_1$line[not_fixated]
            temp$word_line<- sent_1$word_line[not_fixated]
            temp$wordID<- sent_1$wordID[not_fixated]
            
            # merge fixated and non-fixated data frames:
            raw_fix_new<- rbind(raw_fix_new, temp)
            
          }
          
        } # end of s loop
        
      } # end of  if addNonFixatedWords
      
      try(raw_fix_new$filename<- dataASC[i])
      
      # merge trial-level data:
      raw_fix<- rbind(raw_fix, raw_fix_new)
      
      cat(toString(j)); cat(" ")
    } # end of item (j loop)
    
    cat("\n DONE \n \n");
  } # end of subject
  
  
  ### Add some columns useful for data processing:
  cat("\n Calculating some useful measures for data processing. Hang tight... \n \n")

  raw_fix_new<- NULL

  raw_fix$prev_RS<- NA
  raw_fix$next_RS<- NA
  raw_fix$prevChar<-NA
  raw_fix$nextChar<- NA
  raw_fix$prevX<- NA
  raw_fix$nextX<- NA
  raw_fix$prevY<- NA
  raw_fix$prev_max_char_line<- NA#
  
  nsubs<- unique(raw_fix$sub)
  
  
  for(i in 1: length(nsubs)){
    n<- subset(raw_fix, sub==nsubs[i])
    nitems<- unique(n$item)
    cat(i); cat(" ")

    for(j in 1:length(nitems)){
      m<- subset(n, item== nitems[j])

      l1<- subset(m, line==1)
      max_l1<- l1$max_char_line[1]

      for(k in 1:nrow(m)){
        if(k==1){
          m$prev_RS[k]<- 0
          m$next_RS[k]<- 0
          m$next_RS[k+1]<- 0

          ####
          m$nextChar[k]<- m$char_line[k+1] # next char
          m$nextX[k]<- m$xPos[k+1]

        }else{
          # return sweep stuff
          if(!is.na(m$Rtn_sweep[k])){
            if(m$Rtn_sweep[k]==1){
              m$prev_RS[k-1]<- 1

              if(k+1 <= nrow(m)){
                m$next_RS[k+1]<- 1
              }

            }else{
              m$prev_RS[k-1]<- 0

              if(k+1 <= nrow(m)){
                m$next_RS[k+1]<- 0
              }
            }
          }

          ###
          m$prevChar[k]<- m$char_line[k-1] # prev char
          m$prevX[k] <- m$xPos[k-1] # prev x
          m$prevY[k]<- m$yPos[k-1]

          if(k+1<= nrow(m)){
            m$nextChar[k]<- m$char_line[k+1] # next char
            m$nextX[k]<- m$xPos[k+1] # next x
          }


        }

        if(k== nrow(m)){
          m$prev_RS[k]<- 0
        }

        ## map previous line length (for launch site calculation):
        if(k>1){
          m$prev_max_char_line[k]<- m$max_char_line[k-1] 
        }else{
          m$prev_max_char_line[k]<- NA
        }


      } # end of m
      raw_fix_new<- rbind(raw_fix_new, m)
    } # end of j


  }

  raw_fix<- raw_fix_new;
  rm(raw_fix_new)
  
  # calculate some useful measures:
  try(raw_fix$launchSite<- raw_fix$prev_max_char_line- raw_fix$prevChar)
  try(raw_fix$landStart<- raw_fix$char_line)
  try(raw_fix$undersweep_prob<- ifelse(raw_fix$Rtn_sweep_type=="undersweep", 1, 0))
  
  # map fixations that occurred outside text area to "characters":
  a<- which(is.na(raw_fix$char_line) & !is.na(raw_fix$xPos)& raw_fix$outOfBnds==0) 
  x_offset<- coords$x1[1] # text offset 
  ppl<- coords$x2[1]- coords$x1[1] # number of pixels per letter (width)
  try(raw_fix$landStart[a]<- ceiling((raw_fix$xPos[a]- x_offset)/ppl))

  
  cat("\n \n Re-alignment statistics:\n");
  cat(sprintf("Re-aligned %d out of %d fixations (%f percent)\n\n Subject breakdown\n\n",
              length(which(raw_fix$reMapped==1)),
              nrow(raw_fix), length(which(raw_fix$reMapped==1))/nrow(raw_fix)*100))
  
  # subject statistics:
  subs<- unique(raw_fix$sub)
  perc<- NULL
  
  for(s in 1: length(subs)){
    n<- subset(raw_fix, sub== subs[s])
    perc[s]<- round(length(which(n$reMapped==1))/nrow(n),3)*100
  }
  
  try(dfS<- data.frame(sub=subs, percent_reAligned= perc))
  
  print(dfS)
  
  cat("\n \n All Done!");
  
  # if(sum(raw_fix$hasText)==nrow(raw_fix)){ # if all trials have text, remove column
  #   raw_fix$hasText<- NULL
  # }
  
  
  return(raw_fix)
}

