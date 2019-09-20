
# Martin R. Vasilev, 2017


########################
#  UTILITY FUNCTIONS   #
########################

# Short functions:
get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}

isInside2D<- function(value, x1, x2){ value>= x1 & value <= x2}


# get a list of .asc files from a user-provided directory
get_files<- function(dir= "C:/Users/Martin Vasilev/Documents/Test", file_ext= ".asc"){
   
  if(dir== ""){
      dir= getwd()
  }
  # get a list of all file in dir:
  all_files<- list.files(dir)
  # remove non-asc files (if present)
  all_files<- all_files[grepl(file_ext, all_files)]
  # remove txt files (of present):
  all_files<- all_files[!grepl(".txt", all_files)]

  # sort files by number in string
  get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
  num<- get_num(all_files)

  if(!is.na(num[1])){
    all_files<- all_files[order(num, all_files)]
  }
  # convert to directory string for each data file:
  if(length(all_files)>0){
    all_dirs<- NULL
    for(i in 1:length(all_files)){
      all_dirs[i]<- paste(dir, "/", all_files[i], sep = "")
    }

    message(paste("Found", toString(length(all_files)), file_ext, "files in the specified directory!", "\n"))
    return(all_dirs)
  }else{
    stop(paste("Found no", file_ext, "files in the specified directory!"))
  }
} # end of get_files()


# Extract text containing stimuli info:
get_text<- function(file){ ## extracts the loaded text material                         #

  start<- which(grepl("DISPLAY TEXT", file))+1 # start point
  end<- which(grepl("START", file))

  if(length(start)==0){ # no start of text detected
    return(0) # don't map fixations to stimuli

  } else{ # extract text containing stimuli info
    text<- file[start:end] # subset file
    a= which(gregexpr(pattern ='BUTTON', text)==1 | gregexpr(pattern ='INPUT', text)==1)

    if(length(a)>0){
      text<- text[-a]
      # text<- text[-a] # because one line is removed
    }

    #input<- which(grepl("INPUT", text)); input<- input[1]-1 # end point
    end2<- which(grepl("REGION", text))
    end2<- end2[length(end2)]

    text<- text[1:end2]

    #end<- which(grepl("INPUT", file)); end<- end[1]-1 # end point
    #text<- file[start:end] # subset file
    return(text)

  }

  }


# Create a database with position of text on screen
get_coord<- function(string){ # extracts text coordinates from trial info

  # check to make sure there are no button press flags here..
  a= which(gregexpr(pattern ='BUTTON', string)==1)

  if(length(a)>0){
    string<- string[-a]
  }

  out <-  do.call( rbind, strsplit( string, '\t' ) )
  out<- out[,2]
  out <- suppressWarnings(data.frame(do.call(rbind, strsplit(out, ' ' ))))

  out<- subset(out, X2!="DELAY") # Remove DELAY 1ms lines (Eyetrack)

  out$X7<- as.numeric(as.character(out$X7))
  out$X8<- as.numeric(as.character(out$X8))
  out$X9<- as.numeric(as.character(out$X9))
  out$X10<- as.numeric(as.character(out$X10))
  out$X11<- as.numeric(as.character(out$X11))

  fix_spaces<- function(out){
    out$space<- NULL
    a<- which(out$X6=="") # find position of empty spaces
    out$space<- NA
    out$space[a]<- 1
    # Re-arrange misplaced coords
    out$X7[a]<- out$X8[a]
    out$X8[a]<- out$X9[a]
    out$X9[a]<- out$X10[a]
    out$X10[a]<- out$X11[a]
    out<- out[,-11]
  }

  out<- fix_spaces(out)
  out<- out[,-c(1,2,3,5)] # remove useless columns
  
  
  # map sentences:
  map_sent<- function(out){
    sent_bnd<-  which(out$X6=="."| out$X6=="?");
    sent<- NULL

    if(length(sent_bnd)>0){
      for(i in 1:length(sent_bnd)){
        if(i==1){
          sent[1:sent_bnd[i]]<- 1
        }
        else{
          sent<- c(sent, c(rep(i, length(sent_bnd[i-1]+1:sent_bnd[i])- length(sent))))
          #  sent[sent_bnd[i-1]+1:sent_bnd[i]]<- i
        }
        if(i==length(sent_bnd)){
          sent<- c(sent, c(rep(i+1, length(sent_bnd[i]+1:nrow(out))- length(sent))))
        }
      }
      out$sent<- sent
    } else{
      out$sent<- 1
    }

    return(out)
  }

  out<- map_sent(out)

  # map lines:
  map_line<- function(out){
    line<- NULL
    lines<- sort(unique(out$X8));
    # as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
    #lines<- as.numeric.factor(lines)

    for(i in 1:length(lines)){
      loc_lines<- which(out$X8==lines[i])
      line<- c(line, rep(i, length(loc_lines)))
      out$space[length(line)]<- 2
    }
    out$line<- line

    return(out)
  }

  out<- map_line(out)


  # map words:
  map_words<- function(out){

    out$X6<- as.character(out$X6)
    out$word<- NULL
    curr_sent=1
    curr_word=1
    sent_line<- which(out$space==2); sent_line<- sent_line+1

    for(i in 1:nrow(out)){

      newSent<- curr_sent!= out$sent[i]

      out$word[i]<- curr_word
      if(out$X6[i]== ""& !newSent){
        curr_word<- curr_word+1
        out$word[i]<- curr_word
      }


      if(is.element(i, sent_line)){
        if(out$X6[i]!="."){
          curr_word<- curr_word+1
        }
        out$word[i]<- curr_word
      }

      if(newSent){
        curr_sent<- curr_sent+1
        curr_word<- 1
        out$word[i]<- curr_word
      }

    }

    return(out)
  }

  out<- map_words(out)
  
  # map word per line:
  
  out$word_line<- NA
  curr_line<- out$line[1]
  curr_word<- out$word[1]
  
  for(i in 1:nrow(out)){
    
    if(out$line[i]> curr_line){
      curr_line<- curr_line+1
      curr_word<- 1
    }
    
    if(!is.na(out$space[i])){
      if(out$space[i]==1){
        curr_word<- curr_word+1
      }
      
    }
    
    out$word_line[i]<- curr_word
  }
  

  # change column names:
  colnames(out)<- c("char", "letter", "x1", "y1", "x2", "y2", "space", "sent",
                    "line", "word", "word_line")
  
#  if(sum(out$space)-1==nrow(out)){
#    out$line_char<- NA
#    out$wordID<- NA; out$char_word<- NA;
#    return(out)
#  }
  
  
  # map characters per line (for Eye Doctor):
  out$line_char<- NA
  unq_line<- unique(out$line)
  for(i in 1:length(unq_line)){
    line<- which(out$line==unq_line[i])
    out$line_char[line[1]:line[length(line)]]<- 1:length(line)
  }
  
  
  # word identity and char per word:
  out$wordID<- NA; out$char_word<- NA; word_list<- NULL; cols<- NULL
  sent_list= unique(out$sent)
  
  for(i in 1: length(sent_list)){ # for each sentence
    word_list<- unique(out$word[which(out$sent==i)])
    
    for(j in 1:length(word_list)){
      cols<- which(out$sent == i & out$word == j)
      out$wordID[cols]<- paste(out$letter[cols], collapse = "")
      
      if(out$letter[cols[1]]==""){
        out$char_word[cols]<- 0:(length(cols)-1)
      }else{
        out$char_word[cols]<- 1:length(cols)
      }
      
    }
  }

  return(out)
}


mapTextSL<- function(sent, ppl= 11, xOffset= 50, ResY= 1080){
  
  ###
  char<- 0:(nchar(string)-1)
  letter<- unlist(strsplit(string, ''))
  space<- rep(NA, nchar(string))
  space[which(letter==" ")]<- 1
  line<- rep(1, nchar(string))
  line_char<- char+1
  y1<- rep(ResY/2, nchar(string))- ppl*2
  y2<- rep(ResY/2, nchar(string))+ ppl*2
  x1<- rep(NA, nchar(string))
  x2<- rep(NA, nchar(string))
  sent<- rep(NA, nchar(string))
  word<- rep(NA, nchar(string))
  wordID<- rep(NA, nchar(string))
  char_word<- rep(NA, nchar(string))
  
  coords<- data.frame(char, letter, x1, y1, x2, y2, space, sent,
                      line, word, line_char, wordID, char_word)
  
  count<- xOffset
  word_count<- 1
  sent_count<- 1
  for(j in 1:nrow(coords)){
    if(coords$letter[j]== " "){
      word_count<- word_count+1
    }
    coords$sent[j]<- sent_count  
    coords$word[j]<- word_count
    coords$x1[j]<- count
    coords$x2[j]<- count+ ppl
    count<- count + ppl
    
    if(coords$letter[j]== "." | coords$letter[j]== "!" | coords$letter[j]== "?"){
      word_count<- 1 # new sentence, reset word count
      sent_count<- sent_count+1 # next sent
    }
  }
  
  
  # word identity and char per word:
  word_list<- NULL; cols<- NULL
  sent_list= unique(coords$sent)
  
  for(i in 1: length(sent_list)){ # for each sentence
    word_list<- unique(coords$word[which(coords$sent==i)])
    
    for(j in 1:length(word_list)){
      cols<- which(coords$sent == i & coords$word == j)
      coords$wordID[cols]<- paste(coords$letter[cols], collapse = "")
      
      if(coords$letter[cols[1]]==""){
        coords$char_word[cols]<- 0:(length(cols)-1)
      }else{
        if(j==1 & i==1){
          coords$char_word[cols]<- 1:length(cols)
        }else{
          coords$char_word[cols]<- 1:length(cols)-1
        }
        
      }
      
    }
  }
  return(coords)
}



# Use stimuli information to create a coordinate map for each pixel on the screen
# This makes it possible to find exactly what participants were fixating
coord_map<- function(coords, x=ResX, y= ResY){

  coords$id<- 1:nrow(coords)
  map<- data.frame(matrix(NA, nrow = y, ncol = x))

  for(i in 1:nrow(coords)){
    map[coords$y1[i]:coords$y2[i],coords$x1[i]:coords$x2[i]]<- coords$id[i]

  }

  return(map)
}


# Create a data frame with information about trials, to be used for pre-processing
# trial_info<- function(file, maxtrial, data){ # extracts information for processing trials
#   ### get trial names:
#   ID<- which(grepl('TRIALID', file));
#   
#   ### get start & end times
#   start<- which(grepl('DISPLAY ON', file))
#   end <- which(grepl('DISPLAY OFF', file))
#   
#   trial_text<- file[ID]
#   trials<- substr(trial_text, unlist(gregexpr(pattern =' ',trial_text[1]))[2]+1, nchar(trial_text))
#   #trials<- trials[which(letter!="P" &  letter!="F")] # remove practice items and questions
#   trials<- gsub(" ", "", trials)
#   # sometimes there is an extra empty space that can mess up detection of duplicates
#   
#   # check for duplicate trials (e.g., failed gaze box)
#   dupl<- unique(trials[duplicated(trials)])
#   
#   if(length(dupl)>0){
#     out<- NULL
#     for(d in 1:length(dupl)){
#       a<- which(trials== dupl[d])
#       out<- c(out, a[1:length(a)-1])
#     }
#     
#     trials<- trials[-out]
#     ID<- ID[-out]
#     
#     message(paste(" Duplicated trial", dupl, "\n"))
#     message("Analysing only last attempt at the trial(s)!")
#   }
#   if(length(start!= length(trials)) | length(end)!= length(trials)){
#     keep<- end-start>200
#     start<- start[keep]
#     end<- end[keep]
#   }
# 
# 
#   
# #  pract<- trials[grepl("P", trials)]
# #  if(length(pract)>1){
# #    trials<- trials[!is.element(trials, pract)]
# #  }
# 
#   ### get condition:
#   I<- unlist(gregexpr(pattern ='I',trials)) # start of item info
#   cond<- as.numeric(substr(trials, 2, I-1)) # extract condition number
# 
#   ### get item:
#   D<- unlist(gregexpr(pattern ='D',trials)) # start of dependent info
#   item<- as.numeric(substr(trials, I+1, D-1)) # extract condition number
#   depend<- as.numeric(substr(trials, nchar(trials), nchar(trials)))
#   
#   # get letter:
#   E<- substr(trials, 1, 1)
# 
#   ### get sequence:
#   #seq<- 1:length(trials)
# 
#   # duplicated<- trials[duplicated(trials)]
# 
#   # if(length(duplicated)>0){ # if there were aborted trials..
#   #   message(paste(" Duplicated trial", duplicated, "\n"))
#   #   message("Analysing only last attempt at the trial(s)!")
#   # 
#   #   toBeRemoved<- NULL
#   # 
#   #   for(i in 1:length(duplicated)){
#   #     dup_rem<- which(trials==duplicated[i])
#   # 
#   #     for(j in 1:length(dup_rem)){
#   #       if(j!=length(dup_rem)){
#   #         toBeRemoved[length(toBeRemoved)+1]= dup_rem[j]
#   #       }
#   #     } # end of j
#   #   } # end of i
#   # 
#   #   #start<- start[-toBeRemoved]
#   #   # end<- end[-toBeRemoved]
#   #   cond<- cond[-toBeRemoved]
#   #   item<- item[-toBeRemoved]
#   #   E<- E[-toBeRemoved]
#   #   # seq<- seq[-toBeRemoved]
#   #   depend<- depend[-toBeRemoved]
#   #   ID<- ID[-toBeRemoved]
#   #   
#   #   # workaround for missing trial end stamps:
#   #   if(length(end)< length(start)){
#   #     start<- start[-toBeRemoved]
#   #   }
#   #   
#   # } # end of aborted conditional
#   # 
#   #if(length(end)!= length(start) & length(start)== length(cond)){
#   #  end<- end[-length(end)]
#   #}
#   
#   trial_db<- data.frame(cond, item, depend, E, start, end, ID)
#   trial_db<- subset(trial_db, depend==0 & item< maxtrial+1)
#   trial_db$seq<- 1:nrow(trial_db)
#   
#   trial_db<- subset(trial_db, E=="E")
#   trial_db$E<- NULL
#   ###
# 
#   return(trial_db)
# }

trial_info<- function(file, maxtrial, trial_flag= "TRIALID", trial_start_flag= "SYNCTIME", selectEXP= TRUE){
  
  ##
  get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
  
  ##
  parse_itemID<- function(trials){
    I<- unlist(gregexpr(pattern ='I',trials)) # start of item info
    cond<- as.numeric(substr(trials, 2, I-1)) # extract condition number
    
    ### get item:
    D<- unlist(gregexpr(pattern ='D',trials)) # start of dependent info
    item<- as.numeric(substr(trials, I+1, D-1)) # extract condition number
    depend<- as.numeric(substr(trials, nchar(trials), nchar(trials)))
    
    # get letter:
    E<- substr(trials, 1, 1)
    
    return(c(E, cond, item, depend))
    
  }
  
  # get the trial ID stamps
  trial_time<- which(grepl(trial_flag, file))
  trial_text<- file[trial_time]
  trials<- substr(trial_text, unlist(gregexpr(pattern =' ',trial_text[1]))[2]+1, nchar(trial_text))
  trials<- gsub(" ", "", trials)
  
  start_time<- which(grepl(trial_start_flag, file))
  
  # if there is no trial start flag, use trial ID flag:
  if(length(start_time)==0){
    start_time= trial_time
  }
  
  # end of last file is end of file
  end_time<- c(trial_time[2:length(trial_time)], length(file))-1
  
  ### parse trials (takes care of duplicates, aborted trials, etc.):
  if(length(start_time)!= length(trial_time)){ # mismatch problems..
    all<- NULL
    for(k in 1:length(trial_time)){
      t<- which(trial_time[k]<start_time & end_time[k]>start_time)
      if(length(t)>0){
        all<- c(all, k)
      }else{
        #print(k)
      }
      #
    }
    trial_time= trial_time[all]
    end_time= end_time[all]
    trials= trials[all]
  } 
  
  trial_db<- NULL
  
  for(i in 1:length(trials)){
    v= parse_itemID(trials[i])
    temp<- data.frame(cond= v[2], item= v[3], depend= v[4], E= v[1], start= start_time[i],
                      end= end_time[i], ID= trial_time[i], 
                      stamp= trials[i], keep= 1)
    temp$stamp<- as.character(temp$stamp)
    temp$item<- as.numeric(as.character(temp$item))
    temp$cond<- as.numeric(as.character(temp$cond))
    temp$depend<- as.numeric(as.character(temp$depend))
    temp$E<- as.character(temp$E)
    
    dupl<- which(trial_db$stamp== temp$stamp)
    if(length(dupl)>0){
      trial_db$keep[dupl]=0
      message(paste(" Duplicated trial ", temp$stamp, "! ",
                    "Analysing only last attempt at the trial!", "\n", sep= ""))
    }
    
    trial_db<- rbind(trial_db, temp)
  }
  
  # if all item numbers are NAs, use trial stamp instead:
  howManyNAs<- length(which(is.na(trial_db$item)))
  if(howManyNAs== nrow(trial_db)){
    trial_db$item<- trial_db$stamp
  }
  
  
  if(!selectEXP){ # keep only questions
    trial_db<- subset(trial_db, depend> 0 & item< maxtrial+1)#, keep==1
    trial_db<- subset(trial_db, E=="F")
  }else{ # keep only experimental items
    if(howManyNAs!= nrow(trial_db)){ # continue only if we use Eyetrack convention
      trial_db<- subset(trial_db, depend==0 & item< maxtrial+1)#, keep==1)
      trial_db<- subset(trial_db, E=="E")
      trial_db$depend<- NULL
    }

  }
  
  
  trial_db$seq<- 1:nrow(trial_db)
  
  trial_db$E<- NULL
  trial_db$stamp<- NULL
  
  trial_db<- subset(trial_db, keep==1)
  trial_db$keep<- NULL
  
  
  return(trial_db)
  
}


# Basic pre-processing and extraction of fixations from data file:
parse_fix<- function(file, map, coords, trial_db, i, ResX, ResY, tBlink,
                     hasText=TRUE, SL= FALSE){

  get_FIX_stamp<- function(string){
    
    num_stamp<- NULL
    for(i in 1:length(string)){
      char_stamp<-substr(string[i], 1, unlist(gregexpr(pattern ='\t', string[i]))[1]-1) 
      num_stamp[i]<- as.numeric(char_stamp)
    }
    # char_stamp<-substr(string, 1, unlist(gregexpr(pattern ='\t', string))[1]-1) 
    # num_stamp <- as.numeric(char_stamp)
    return(num_stamp)
  }

  loc_blinks<- function(blink_time, s_time){findInterval(blink_time, s_time)+1 }
  # +1 because it affects the next fixation

  get_x_pixel<- function(string){
    a<- data.frame(do.call( rbind, strsplit( string, '\t' ) ) )
    x<- as.numeric(as.character(unlist(a$X4)))

    return(x)
  }
  
  # in case of weird formatting (e.g., Cohen's script...)
  get_x_pixel_ALT<- function(string){
    a<- data.frame(do.call( rbind, strsplit( string, ' ' ) ) )
    x<- a[a!=""][6]
    
    return(x)
  }

  get_y_pixel<- function(string){
    a<- data.frame(do.call( rbind, strsplit( string, '\t' ) ) )
    y<- as.numeric(as.character(unlist(a$X5)))
    
    return(y)
  }
  
  # in case of weird formatting (e.g., Cohen's script...)
  get_y_pixel_ALT<- function(string){
    a<- data.frame(do.call( rbind, strsplit( string, ' ' ) ) )
    y<- a[a!=""][7]
    return(y)
  }


  get_seq<- function(start, end, file, prev= F){
    
    if(!prev){
      # find start:
      lines<- grep(start, file)
      hits<- file[lines]
      loc<- grep("SFIX", hits)
      s1<- lines[loc] +1
      
      # find end:
      lines2<- grep(end, file)
      hits2<- file[lines2]
      loc2<- grep("EFIX", hits2)
      #hits2<- hits2[-loc2]
      s2<- lines2
      s2<- s2[-loc2]
      
      # extract fixation txt data:
      text<- file[s1:s2]
      out <-  as.data.frame(do.call(rbind, strsplit( text, '\t' )))
      out$V2<- as.numeric(as.character(out$V2))
      out$V3<- as.numeric(as.character(out$V3))
      out$V4<- as.numeric(as.character(out$V4))
      
    }else{
      # find start:
      s1<- grep(start, file)
      if(length(s1)>1){
        s1<- s1[1]
      }
      
      
      # find end:
      s2<- grep(end, file)
      if(length(s2)>1){
        s2<- s2[1]
      }
      
      text<- file[s1:s2]
      
      whichSSACC<- which(grepl('SSACC', text))
      whichESACC<- which(grepl('ESACC', text))
      whichSFIX<- which(grepl('SFIX', text))
      whichEFIX<- which(grepl('EFIX', text))
      whichMSG<- which(grepl('MSG', text))
      whichSBLINK<- which(grepl('SBLINK', text))
      whichEBLINK<- which(grepl('EBLINK', text))
      
      allOut<- c(whichSSACC, whichESACC, whichSFIX, whichEFIX, whichMSG,
                 whichSBLINK, whichEBLINK)
      
      if(length(allOut)>0){
        text<- text[-allOut]
      }
      
      out <-  as.data.frame(do.call(rbind, strsplit( text, '\t' )))
      out$V2<- as.numeric(as.character(out$V2))
      out$V3<- as.numeric(as.character(out$V3))
      out$V4<- as.numeric(as.character(out$V4))
      
    }
    
    return(out)

  }

  trialFile<- file[trial_db$ID:trial_db$end]


  # get position of fixation stamps:
  SFIX_stamps<- which(grepl('SFIX', file[trial_db$start:trial_db$end]))
  EFIX_stamps<- which(grepl('EFIX', file[trial_db$start:trial_db$end]))
  
  # # get position of fixation stamps:
  # SSACC_stamps<- which(grepl('SSACC', file[trial_db$start:trial_db$end]))
  # ESACC_stamps<- which(grepl('ESACC', file[trial_db$start:trial_db$end]))
  
  #which(ESACC_stamps>= )

  if(length(SFIX_stamps)==0 | length(EFIX_stamps)==0){
    # means that there was no complete fixation on this trial (i.e,
    # participant likely pressed end button by mistake)
    raw_fix<- NULL
    return(raw_fix)
    message(sprintf("No fixations in Trial %d: excluded", j))
  }

  if(EFIX_stamps[1]<SFIX_stamps[1]){ # removes fixation that triggered gaze box
    EFIX_stamps<- EFIX_stamps[-1]
  }
  
  # if(ESACC_stamps[1]<SSACC_stamps[1]){ #same for sacc
  #   ESACC_stamps<- ESACC_stamps[-1]
  # }

  if(EFIX_stamps[length(EFIX_stamps)]< SFIX_stamps[length(SFIX_stamps)]){
    SFIX_stamps<- SFIX_stamps[-length(SFIX_stamps)]
  } # fixation was not terminated before the end of trial
  
  # if(ESACC_stamps[length(ESACC_stamps)]< SSACC_stamps[length(SSACC_stamps)]){
  #   SSACC_stamps<- SSACC_stamps[-length(SSACC_stamps)]
  # } # sacc was not terminated before the end of trial
  
  parse_sacc<- function(string){a<- unlist(strsplit(string, "\t")); return(as.numeric(a[3]))}
  
  esacc_flag<- file[SFIX_stamps+ trial_db$start-2]
  saccDur<- NULL
  for(k in 1:length(esacc_flag)){
    saccDur[k]<- parse_sacc(esacc_flag[k])
  }
  
  # get start and end time of fixations:
  s_time<- get_FIX_stamp(file[SFIX_stamps+ trial_db$start])  # start time of fixation
  e_time<- get_FIX_stamp(file[EFIX_stamps+ trial_db$start-2]) # end time of fixation
  
  # # get start and end time of saccade:
  # s_time_sacc<- get_FIX_stamp(file[SSACC_stamps+ trial_db$start])  # start time of sacc
  # e_time_sacc<- get_FIX_stamp(file[ESACC_stamps+ trial_db$start-1]) # end time of sacc

  # calculate fixation durations:
  fixDur<- e_time- s_time
  # saccDur<- e_time_sacc- s_time_sacc

  # get x pixel position:
  x<- get_x_pixel(file[EFIX_stamps+ trial_db$start-1])
  if(length(x)!= length(fixDur)){
    x<- NULL
    for (m in 1:length(fixDur)){
     x<- c(x, as.numeric(get_x_pixel_ALT(file[EFIX_stamps+ trial_db$start-1][m])))  
    }
  }

  # get y pixel position:
  y<- get_y_pixel(file[EFIX_stamps+ trial_db$start-1])
  if(length(y)!= length(fixDur)){
    y<- NULL
    for (m in 1:length(fixDur)){
      y<- c(y, as.numeric(get_y_pixel_ALT(file[EFIX_stamps+ trial_db$start-1][m])))  
    }
  }

  # new blink code:
  blink<- NULL
  prev_blink<- NULL
  after_blink<- NULL
  
  
  for(k in 1:length(s_time)){
    
    # don't check if either stamp is missing
    if(is.na(s_time[k]) | is.na(e_time[k])){
      blink[k]<- NA
      after_blink[k]<- NA
      prev_blink[k]<- NA
      next
    }
    
    GP<- suppressWarnings(get_seq(s_time[k], e_time[k], trialFile))
      
    eye_clozed<- which(GP$V4==0)
    if(length(eye_clozed)>0){
      blink[k]<- 1
    }else{
      blink[k]<- 0
    }
    
    
    # blink after start of fix?
    if(k!= length(s_time)){ #if not last fix on trial
      if(length(grep(e_time[k]+tBlink, trialFile))>0){
        after<- suppressWarnings(get_seq(e_time[k], e_time[k]+tBlink, trialFile, prev=T))
        after_closed<- which(after$V4==0)
        if(length(after_closed)>0){
          after_blink[k]<- 1
        }else{
          after_blink[k]<- 0
        }
      } else{ # if not enough data..
        after_blink[k]<- NA
      }
      
    }else{ # na, no data after this fix
      after_blink[k]<- NA
    }
    
    
    # blink before start of fix?
    if(k>1){
      #Start_trial<- which(grepl(s_time[k]-tBlink, trialFile)) # check if this time exists in trial:
      prev<- suppressWarnings(get_seq(s_time[k]-tBlink, s_time[k], trialFile, prev=T))
      
      prev_closed<- which(prev$V4==0)
      if(length(prev_closed)>0){
        prev_blink[k]<- 1
      }else{
        prev_blink[k]<- 0
      }
    }else{
      prev_blink[k]<- NA
    }
  


  } # end of k


  # find blinks:
#  blink_stamp<- which(grepl('EBLINK', file[trial_db$start:trial_db$end]))
#  blink_time<- get_FIX_stamp(file[blink_stamp+ trial_db$start])-1
#  blink_out<- which(blink_time<s_time[1]| blink_time>e_time[length(e_time)])

#  if(length(blink_out>0)){ # blinks outside time window that is analysed
#    blink_time<- blink_time[-blink_out]  # remove them
#  }

#  blink_pos<- loc_blinks(blink_time, s_time)
#  blink<- rep(0, length(s_time))
#  blink[blink_pos]<-1

  # merge into a dataframe:
  fix<- data.frame(s_time, e_time, fixDur, saccDur, x, y, blink, prev_blink, after_blink)
  # fix$x<- as.numeric(as.character(fix$x))
  # fix$y<- as.numeric(as.character(fix$y))

  #-----------------------------------------------#
  #    map fixations to stimuli on the screen:    #
  #-----------------------------------------------#

  loc<- NULL; raw_fix<- NULL; temp<- NULL; sub<- NULL; prev_blink<- NULL
  SFIX<- NULL; EFIX<- NULL; xPos<- NULL; yPos<- NULL; after_blink<- NULL
  item<- NULL; cond<- NULL; seq<- NULL; fix_num<- NULL; fix_dur<- NULL; sacc_dur= NULL
  sent<- NULL; line<- NULL; word<- NULL; char_trial<- NULL; char_line<- NULL; word_line<- NULL
  max_sent<- NULL; max_word<- NULL; intersent_regr<- NULL; regress<- NULL
  intrasent_regr<- NULL; blink<- NULL; outOfBnds<- NULL; outsideText<- NULL
  wordID<- NULL; land_pos<- NULL; sacc_len<- NULL

  if(hasText){
    # max word for each sentence:
    curr_sent<- matrix(0, max(coords$sent),2)
    curr_sent[,1]<- c(1:max(coords$sent))
  }


  for(j in 1:nrow(fix)){

    if(hasText){
      if(round(fix$y[j])>0 & round(fix$x[j])>0 & round(fix$y[j])<= ResY & round(fix$x[j])<= ResX){ # to prevent negative numbers
        loc<- map[fix$y[j], fix$x[j]] # locate fixation
      }else{
        loc<- NA # workaround to get same result as out of screen
      }
      
    }

    # general info:
    sub[j]<- i
    item[j]<- trial_db$item
    cond[j]<- trial_db$cond
    seq[j]<- trial_db$seq
    fix_num[j]<- j
    fix_dur[j]<- fix$fixDur[j]
    sacc_dur[j]<- fix$saccDur[j]

    # info from asc file:
    SFIX[j]<- fix$s_time[j];
    EFIX[j]<- fix$e_time[j]
    xPos[j]<- fix$x[j];
    yPos[j]<- fix$y[j]
    blink[j]<- fix$blink[j]
    prev_blink[j]<- fix$prev_blink[j]
    after_blink[j]<- fix$after_blink[j]


      if(xPos[j]<1 | xPos[j]> ResX | yPos[j]< 1 | yPos[j]>ResY){ # fixation is out of bounds
        outOfBnds[j]<- 1
      } else{
        outOfBnds[j]<- 0

        if(hasText){
          # outside of text area?
          if(is.na(loc)){
            outsideText[j]<- 1
          } else{
            outsideText[j]<- 0
          }
        }
      }

    if(fix$x[j]<0){
      loc<- NA
      outOfBnds[j]<- 1
      outsideText[j]<- 1
    }


    if(hasText){
      # stimuli info:
      if(!is.null(loc) & !is.na(loc)){
        sent[j]<- coords$sent[loc]
        line[j]<- coords$line[loc]
        word[j]<- coords$word[loc]
        word_line[j]<- coords$word_line[loc]
        char_trial[j]<- as.numeric(as.character(levels(coords$char[loc])[coords$char[loc]]))+1
        char_line[j]<- coords$line_char[loc]
        wordID[j]<- coords$wordID[loc]
        land_pos[j]<- coords$char_word[loc]
        
        if(j>1){
          sacc_len[j]<- abs(char_trial[j]- char_trial[j-1])
        }else{
          sacc_len[j]<- NA
        }
        
        # +1 bc Eyetrack counts from 0
      } else{
        sent[j]<- NA; line[j]<- NA; word[j]<- NA; word_line[j]<- NA; char_trial[j]<- NA; char_line[j]<- NA
        wordID[j]<- NA; land_pos[j]<- NA; sacc_len[j]<- NA
      }

      # saccade stuff:
      if(j==1){
        max_sent[j]<- sent[j]
      } else{
        max_sent[j]<- max_sent[j-1]

        if(!is.na(max_sent[j])& !is.na(sent[j]) & sent[j]> max_sent[j]){
          max_sent[j]<- sent[j]
        }
      }

      # maximum word:
      if(j==1){
        max_word[j]<- abs(word[j])
        curr_sent[sent[j],2]<- abs(word[j])
      } else{
        max_word[j]<- curr_sent[sent[j],2]
        if(!is.na(word[j])& abs(word[j])>curr_sent[sent[j],2]){
          max_word[j]<- abs(word[j])
          curr_sent[sent[j],2]<- abs(word[j]) # replace new max word
        }
      }

      # Regression stuff:
      #if(!is.na(max_sent[j])& !is.na(sent[j]) & sent[j]< max_sent[j]){
      #  intersent_regr[j]<- 1
      #} else{
      #  intersent_regr[j]<- 0
      #}

      # intra-sentence regressions:
      if(!is.na(word[j])& abs(word[j])<max_word[j]){
      #  intrasent_regr[j]<- 1
        regress[j]<- 1
      }else{
        regress[j]<- 0
      #  intrasent_regr[j]<- 0
      }
      
      if(j>1 & !is.na(word[j])){
        if(abs(word[j])== max_word[j] & regress[j-1]==1 & is.element(word[j], unique(word[1:(j-1)]))){
          regress[j]<- 1
        }
      }

    } else{ # end of if hasText
      sent[j]=NA; max_sent[j]=NA; line[j]=NA; word[j]=NA; max_word[j]=NA;
      char_trial[j]=NA; intrasent_regr[j]=NA; intersent_regr[j]=NA; outsideText[j]=NA;
      char_line[j]= NA; regress[j]<- NA; wordID[j]<- NA; land_pos[j]<- NA; sacc_len<- NA
    }




  } # end of j loop

  if(length(outsideText)!= length(item)){
    outsideText[length(outsideText):length(item)]<- NA
  }

#  raw_fix<- data.frame(sub,item, cond, seq, s_time, e_time,xPos, yPos, fix_num, fix_dur,
#                       sent, max_sent, line, word, max_word, char_trial, intrasent_regr, intersent_regr, blink,
#                       outOfBnds, outsideText)
  if(SL){
    raw_fix<- data.frame(sub,item, cond, seq, SFIX, EFIX, xPos, yPos, fix_num, fix_dur, sacc_dur,
                         sent, line, word, word_line, char_trial, char_line, regress, wordID, land_pos,
                         sacc_len, blink, prev_blink, after_blink, outOfBnds, outsideText)
  }else{
    raw_fix<- data.frame(sub,item, cond, seq, SFIX, EFIX, xPos, yPos, fix_num, fix_dur, sacc_dur,
                         sent, line, word, word, char_trial, char_line, wordID, land_pos,
                         sacc_len, blink, prev_blink, after_blink, outOfBnds, outsideText)
  }

    #raw_fix<- raw_fix[-nrow(raw_fix),]

  if(hasText==TRUE){
    raw_fix$hasText<-1
  }else{
    raw_fix$hasText<-0
  }


  return(raw_fix)
}


#############
# PLOTFIX.R #
#############

plot_fix<- function(coords, raw_fix_temp, i, j, ResX, ResY, hasText=TRUE, plotSecondPass=FALSE, reAligned=FALSE){

  remap_letters<- function(letter, y){ # adjusted y position for easier reading
    letter<- as.character(letter)
    ascenders<- c("b", "d", "f", "h", "i", "k", "l", "t")
    descenders<- c("g", "j", "p", "q", "y")
    punct<- c(",", ".")
    caps<- which(grepl("[A-Z]",letter))

    which_asc<- which(is.element(letter, ascenders))
    which_desc<- which(is.element(letter, descenders))
    which_punct<- which(is.element(letter, punct))

    y[which_desc]<- y[which_desc]- 2
    y[which_asc]<- y[which_asc]+1
    y[which_punct]<- y[which_punct]-4
    y[caps]<- y[caps]+1
    return(y)
  }

  add.alpha <- function(col, alpha=1){
    if(missing(col))
      stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2,
          function(x)
            rgb(x[1], x[2], x[3], alpha=alpha))
  }

  # create new directory for images:
  mainDir <- getwd()
  imgDir <- "img"
  subDir<- paste("s", i, sep= "")

  if (!file.exists(imgDir)){
    dir.create(file.path(mainDir, imgDir), showWarnings = FALSE)
  }

  if (!file.exists(paste(imgDir, "/", subDir, sep=""))){
    dir.create(file.path(imgDir, subDir), showWarnings = FALSE)
  }

  # create output string
  if(!reAligned){
    output= paste(imgDir, "/", subDir, "/", "Sub_", i, "_", "item_", j, ".png", sep="")
  }else{
    output= paste(imgDir, "/", subDir, "/", "Sub_", i, "_", "item_", j, "_ReAligned.png", sep="")
  }



  # open file for plotting:
  png(filename = output, width = ResX, height = ResY,
      units = "px", pointsize = 12, bg="white", res = 100)
  par(mar=c(0,0,0,0)) # set plot margins to 0


  # create empty plot:
  plot(NA, axes=F, main="", xlab="" ,ylab="", col="black", ylim=c(0, ResY), xlim=c(0, ResX), xaxs="i", yaxs="i")

  if(hasText){
    # convert coordinates to numbers:
    coords$y1<- as.numeric(as.character(coords$y1))
    coords$y2<- as.numeric(as.character(coords$y2))

    coords$x1<- as.numeric(as.character(coords$x1))
    coords$x2<- as.numeric(as.character(coords$x2))

    # Plot stimuli that appeared on screen:
    rect(coords$x1, ResY-coords$y1, coords$x2, ResY-coords$y2, col= "white", border= "#CFCBCB")

    xpos= coords$x1+ (coords$x2- coords$x1)/2
    ypos= ResY-coords$y1- (coords$y2- coords$y1)/2

    # correct y pos of some letter for better readibility:
    ypos<- remap_letters(coords$letter, ypos)

    # print letters:
    text(xpos, ypos, coords$letter, col= "black")

    #symbols(raw_fix$xPos, raw_fix$yPos, circles=rep(0.1, nrow(raw_fix)))

    # add saccades
    # for (k in 1:nrow(raw_fix_temp)){
    #   if(k==1){ next}
    #   x1<- raw_fix_temp$xPos[k-1]+6
    #   y1<- ResY-raw_fix_temp$yPos[k-1]

    #   if(x2>x1){
    #     x2<- raw_fix_temp$xPos[k]-10
    #   } else{
    #    x2<- raw_fix_temp$xPos[k]+10
    #   }

    #   y2<- ResY-raw_fix_temp$yPos[k]

    # fixPosX<- x1+ (x2-x1)/2
    # fixPosY<- y1+ (y2-y1)/2

    #    arrows(x1, y1, x2, y2, col = add.alpha("blue",0.2), lty= 1, lwd=2, length=0.10)

    #text(fixPosX, fixPosY, raw_fix_temp$fix_num, col= add.alpha("blue",0.3))
    # }

    if(plotSecondPass){ # plot first- and second-pass fixations in different colours
      first<- subset(raw_fix_temp, intrasent_regr==0 & intersent_regr==0)
      second<- subset(raw_fix_temp, intrasent_regr==1 | intersent_regr==1)

      # first-pass:
      points(x= first$xPos, y=ResY-first$yPos, pch = 16,  col= add.alpha("green",0.20),
             cex= 0.7*(first$fix_dur/75))
      points(x= first$xPos, y=ResY-first$yPos, pch = 16, cex=0.7, col="green")

      # second-pass:
      points(x= second$xPos, y=ResY-second$yPos, pch = 16,  col= add.alpha("orange",0.20),
             cex= 0.7*(second$fix_dur/75))
      points(x= second$xPos, y=ResY-second$yPos, pch = 16, cex=0.7, col="orange")

    } else{ # plot all fixations with the same colour
      # plot fixation durations:
      if(!reAligned){
        points(x= raw_fix_temp$xPos, y=ResY-raw_fix_temp$yPos, pch = 16,  col= add.alpha("green",0.20),
               cex= 0.7*(raw_fix_temp$fix_dur/75))
        points(x= raw_fix_temp$xPos, y=ResY-raw_fix_temp$yPos, pch = 16, cex=0.7, col="green")
      } else{
        for(i in 1:nrow(raw_fix_temp)){
          #whichCol<- "green"
          if(raw_fix_temp$reAligned[i]=="No"){
            whichCol<- "green"
          }
          if(raw_fix_temp$reAligned[i]=="Yes"){
            whichCol<- "blue"
          }

          if(!exists('whichCol')){
            whichCol<- "black"
          }

          #if(raw_fix_temp$reAligned[i]=="Yes" & abs(raw_fix_temp$line[i]- raw_fix_temp$prevLine[i])>1){
          #  whichCol<- "red"
          #}
          points(x= raw_fix_temp$xPos[i], y=ResY-raw_fix_temp$yPos[i], pch = 16,  col= add.alpha(whichCol,0.20),
                 cex= 0.7*(raw_fix_temp$fix_dur[i]/75))
          points(x= raw_fix_temp$xPos[i], y=ResY-raw_fix_temp$yPos[i], pch = 16, cex=0.7, col=whichCol)
        }
      }

    }

    # plot fixation numbers:
    text(raw_fix_temp$xPos, ResY-raw_fix_temp$yPos+15, raw_fix_temp$fix_num, col= add.alpha("red",0.6))



  } else{ # end of hasText

    # plot fixations:
    points(x= raw_fix_temp$xPos, y=ResY-raw_fix_temp$yPos, pch = 16,  col= add.alpha("green",0.20),
           cex= 0.7*(raw_fix_temp$fix_dur/75))
    points(x= raw_fix_temp$xPos, y=ResY-raw_fix_temp$yPos, pch = 16, cex=0.7, col="green")

    # fixation numbers:
    text(raw_fix_temp$xPos, ResY-raw_fix_temp$yPos+15, raw_fix_temp$fix_num, col= add.alpha("red",0.6))
  }

  # print original label:

  if(!reAligned){
    text(ResX - 0.05*ResX, ResY -ResY*0.02, 'ORIGINAL', col= 'black', face=2, cex= 2)
  } else{
    text(ResX - 0.05*ResX, ResY -ResY*0.02, 'REALIGNED', col= 'red', face=2, cex=2)
  }


  dev.off() # close file

}


reAlign<- function(rawfix, coords, map, ResX, ResY, RSpar){

  #------------------------------------------
  #                Functions:
  #------------------------------------------

  # Check for a return sweep (RS):
  RS<- function(i, rawfix, coords, reqYthresh=TRUE, reqXthresh=TRUE, Ythresh= RSpar[1],
                Xthresh= RSpar[2], threshSimilar= RSpar[3]){

    if(i==1){ # first fixation can't be a return sweep
      return(0)
    }

    ### settings:
    lw<- coords$x2[1]-coords$x1[1] # letter width
    lh<- coords$y2[1]-coords$y1[1] # letter height
    meetXthresh<- FALSE
    meetYthresh<- FALSE
    nextSimilar<- FALSE


    #############################
    #         conditions

    # leftward saccade?
    leftSacc<- rawfix$xPos[i]< rawfix$xPos[i-1]

    # downward saccade?
    downSacc<- rawfix$yPos[i]> rawfix$yPos[i-1]

    if(downSacc & reqYthresh){ # x pixels downwards required for RS
      Ydiff<- lh*Ythresh # threshold to be met
      trueYdiff<- rawfix$yPos[i]- rawfix$yPos[i-1] # Y distance traveled
      meetYthresh<- trueYdiff >= Ydiff
    }

    if(leftSacc & reqXthresh){ # x pixels leftwards required for RS
      Xdiff<- lw*Xthresh # threshold to be met
      trueXdiff<- rawfix$xPos[i-1]- rawfix$xPos[i] # X distance traveled
      meetXthresh<- trueXdiff >= Xdiff
    }

    # next fixation with 'similar' Y pos?
   # if(i!= nrow(rawfix)){ # only if not the last fixation in trial..
   #   howSimilar<- threshSimilar*lh # within x/yth of a letter height
   #   nextSimilar<- rawfix$yPos[i+1]>= rawfix$yPos[i]- howSimilar | rawfix$yPos[i+1]<= rawfix$yPos[i]+ howSimilar
   # }

    ###### Point system:
    #   - leftward saccade: 1 point
    #   - leftward saccade meeting threshold: 1 point
    #   - downward saccade: 2 points
    #   - downward saccade meeting threshold: 1 point
    #   = 5 max points

    maxPoints<- 1 +2
    if(reqYthresh){
      maxPoints<- maxPoints+1
    }

    if(reqXthresh){
      maxPoints<- maxPoints+1
    }

    #----------------------------
    currPoints<- 0 # start with 0

    if(leftSacc){
      currPoints<- currPoints + 1/maxPoints
      if(meetXthresh){
        currPoints<- currPoints+ 1/maxPoints
      }
    }

    if(downSacc){
      currPoints<- currPoints +2/maxPoints
      if(meetYthresh){
        currPoints<- currPoints+ 1/maxPoints
      }
    }

    #if(nextSimilar){
    #  currPoints<- currPoints +1/maxPoints
    #}

   # prob<- currPoints/maxPoints
    return(round(currPoints, 2))

  }

  # Re-map fixation info after re-aligning it:
  reMap<- function(rawfix, i, map, coords, newY=NULL){
    rawfix$reAligned[i]<- 1
    rawfix$prevLine[i]<- rawfix$line[i]
    #rawfix$prevX[i]<- rawfix$xPos[i]
    rawfix$prevY[i]<- rawfix$yPos[i]

    if(hasArg(newX)){ # if new x to be replaced..
      rawfix$xPos[i]<- newX
    }

    if(hasArg(newY)){ # if new y to be replaced..
      rawfix$yPos[i]<- newY
    }

    # new location on the screen:
    loc<- map[rawfix$yPos[i], rawfix$xPos[i]]

    rawfix$sent[i]<- coords$sent[loc]
    rawfix$word[i]<- coords$word[loc]
    rawfix$line[i]<- coords$line[loc]

    return(rawfix)
  }

  #---------------------------------------
  # get some info about position of text:
  #---------------------------------------

  ystart<- coords$y1[1] # start of first line on y-axis
  yend<- coords$y2[nrow(coords)] # end of last line on y-axis
  nlines<- max(coords$line) # number of lines in trial
  letterHeight<- coords$y2[1]- coords$y1[1]

  # start position of each line
  xstart<- matrix(nrow = nlines, ncol = 2, data = 0)
  xstart[1:nlines,1]<- 1:nlines

  ystart<- matrix(nrow = nlines, ncol = 2, data = 0)
  ystart[1:nlines,1]<- 1:nlines


  # end position of each line
  xend<- matrix(nrow = nlines, ncol = 2, data = 0)
  xend[1:nlines,1]<- 1:nlines

  yend<- matrix(nrow = nlines, ncol = 2, data = 0)
  yend[1:nlines,1]<- 1:nlines



  for(i in 1:nlines){
    a<- subset(coords, line==i)
    xstart[i,2]<- a$x1[1]
    xend[i,2]<- a$x2[nrow(a)]

    ystart[i,2]<- a$y1[1]
    yend[i,2]<- a$y2[1]
  }

  lineCenter<- ystart[,2] + letterHeight/2


  ####################################################
  #            Process fixations here:               #
  ####################################################

  rawfix$pRS<- NULL
  rawfix$pILS<- NA
  rawfix$reAligned<- '0'
  rawfix$prevY<- NA
  rawfix$prevLine<- NA


  ## Calculate probability of return sweep for each fixation:
  for(i in 1:nrow(rawfix)){
    rawfix$pRS[i]<- RS(i, rawfix, coords)

    if(i>1){
      if(rawfix$pRS[i]<1 & rawfix$yPos[i]> rawfix$yPos[i-1]+letterHeight/2){
        rawfix$pRS[i]<- 1
      }
    }

    rawfix$prevX[i]<- rawfix$xPos[i]
    rawfix$prevY[i]<- rawfix$yPos[i]

   # check for interline saccade (i.e., returning to previous lines)
    if(i>1){
      if(rawfix$yPos[i]< rawfix$yPos[i-1]-letterHeight/2){
        rawfix$pILS[i]<- 1
      }else{
        rawfix$pILS[i]<- 0
      }
    }

  }

# find return sweep fixations:
RsweepFix<- c(which(rawfix$pRS==1), which(rawfix$pILS==1))
RsweepFix<- sort(RsweepFix)

### re-align fixations according to line passes.
# A line pass is defined as all the fixations that occured between two return sweeps.
# By definition, these must have occured on the same line:

for(i in 0:length(RsweepFix)+1){
  if(i==1){ # first line pass is between first fixation in trial and first return sweep
    linePass<- rawfix[1:RsweepFix[i]-1,]
  } else{
    if(i>length(RsweepFix)){ # fixations after last return sweep on trial
      linePass<- rawfix[RsweepFix[i-1]:nrow(rawfix),]
    }else{
      linePass<- rawfix[RsweepFix[i-1]:RsweepFix[i],]
      linePass<- linePass[-nrow(linePass),]
    }

  }

  if(nrow(linePass)==1){ # skip if only one fix on line (likely exploratory behaviour)
    next
  }

  # take the average y position of all line pass fixations:
  avgYpos<- mean(linePass$yPos,na.rm=T)

  # On which line are we?
  #whichLine<-  findInterval(avgYpos, lineCenter)
  whichLine<- which.min(abs(lineCenter - avgYpos))

  #print(i)
  # re-align fixations (if necessary):
  for(j in 1:nrow(linePass)){
    onLine<- linePass$yPos[j]> ystart[whichLine,2] & linePass$yPos[j]< yend[whichLine,2]

    if(!onLine){
      if(linePass$yPos[j]< ystart[whichLine,2]){ # fixation is above line
          rawfix<- reMap(rawfix, linePass$fix_num[j], map, coords, newY= ystart[whichLine,2]+5)

       }else{ # fixation is below line
          rawfix<- reMap(rawfix, linePass$fix_num[j], map, coords, newY= yend[whichLine,2]-5)
       }
      rawfix$reAligned[linePass$fix_num[j]]<- "Yes" # mark as re-aligned
    } else{
      rawfix$reAligned[linePass$fix_num[j]]<- "No" # mark as not re-aligned
    }

  } # end of re-align loop

} # end of line pass loop


  return(rawfix)

}



isRS<- function(d, l, v, h, l_thresh= 0.33, h_thresh= 0.33){d>= l*l_thresh & v>= h*h_thresh}
