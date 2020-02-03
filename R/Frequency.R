
#' Adds frequency information for words in the text. 
#' 
#' Output includes: Same data frame, but with the added frequency information
#' 
#' @author Martin R. Vasilev
#' 
#' @param data Dataframe containing the raw fixation data that were extracted with the
#' EMreading package
#' 
#' @param database A string indicating which frequency database to use ("SUBTLEX-UK" or "SUBTLEX-US")
#' 
#' 
Frequency<- function(data, database= 'SUBTLEX-UK'){

  cat("This function requires internet connection to download the requested database.\n")
  cat("Attempting to load frequency database. Please wait...\n\n");
  
  if(database== 'SUBTLEX-UK'){
    db<- read.delim(url("https://github.com/martin-vasilev/R_scripts/raw/master/SUBTLEX-UK.csv"), sep = ';')
    word_loc=1
    freq_loc=2
    zipf_loc=6
  }
  
  if(database== 'SUBTLEX-US'){
    db<- read.delim(url("https://github.com/martin-vasilev/R_scripts/raw/master/SUBTLEX-US.csv"), sep = ';')
    word_loc=1
    freq_loc= 2
    zipf_loc= 15
  }
  
  data$freq<- NA
  data$zipf<- NA

  if(!"wordID" %in% colnames(data)){
    stop("wordID column not found in dataframe!")
  }
  
  data$wordID<- as.character(data$wordID)
  
  cat("Mapping frequency to database...\n");
  
  for(i in 1:nrow(data)){
    word<- data$wordID[i]
    loc<- which(db[,1]== word)
    
    if(length(loc)<1){ # remove capital case and try again
      word<- try(tolower(word))
      loc<- which(db[,1]== word)
    }
    
    if(length(loc)<1){ # remove last character and try again
      word<- gsub('[[:punct:] ]+',' ', word)
      word<- gsub(" ", "", word, fixed = TRUE)
      
      loc<- which(db[,1]== word)
    }
    
    if(length(loc)>0){
      
      if(length(loc)>1){
        loc<- loc[1]
      }
      data$freq[i]<- db[loc, freq_loc]
      data$zipf[i]<- db[loc, zipf_loc]
    }
    
    if(i%%1000==0){
      cat(round(i/nrow(data)*100,1)); cat("% ") 
    }
    
  }
  
  return(data)
      
}

