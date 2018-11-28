
trial_db<- function(file, maxtrial){
  get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
  
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
  
  
  trial_flag= "TRIALID"
  trial_start_flag= "SYNCTIME"
  
  # get the trial ID stamps
  trial_time<- which(grepl(trial_flag, file))
  trial_text<- file[trial_time]
  trials<- substr(trial_text, unlist(gregexpr(pattern =' ',trial_text[1]))[2]+1, nchar(trial_text))
  trials<- gsub(" ", "", trials)
  
  start_time<- which(grepl(trial_start_flag, file))
  
  ### parse trials (takes care of duplicates, aborted trials, etc.):
  
  if(length(start_time)== length(trial_time)){ # no mismatch problems..
    
    # end of last file is end of file
    end_time<- c(trial_time[2:length(trial_time)], length(file))-1
    
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
        message(paste(" Duplicated trial", temp$stamp, "\n"))
        message("Analysing only last attempt at the trial(s)!")
      }
      
      trial_db<- rbind(trial_db, temp)
    }
    
    trial_db<- subset(trial_db, depend==0 & item< maxtrial+1)#, keep==1)
    trial_db<- subset(trial_db, E=="E")
    trial_db$seq<- 1:nrow(trial_db)
    
    trial_db$E<- NULL
    trial_db$stamp<- NULL
    trial_db$depend<- NULL
    
    trial_db<- subset(trial_db, keep==1)
    trial_db$keep<- NULL
    
  } else{
    stop("Trials mismatch!")
  }
  
  return(trial_db)
  
}


