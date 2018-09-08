
#' Pre-processing of single-line reading data
#'
#' This function reads in data from .asc files and performs the pre-processing.
#'
#' @author Martin R. Vasilev
#'
#' @param dat data frame containing the raw fixations extracted with the readingET package:
#'
#' @param SL a logical indicating whether this is a single-line experiment (1= yes, 0= no)


ParseData<- function(dat, SL= TRUE){

  ntrials<- NULL; n<- NULL
  for(i in 1:length(unique(dat$sub))){ # for each subject
    ntrials<- unique(dat$item[which(dat$sub==1)])
    ntrials<- sort(ntrials)

    for(j in 1:length(ntrils)){ # for each trial
      n<- subset(dat, sub==i & item== ntrials[j])

      # max word for each sentence:
      curr_sent<- matrix(0, max(n$sent),2)
      curr_sent[,1]<- c(1:max(n$sent))

      if(nrow(n)>0){
        for(k in 1:nrow(n)){ # for each fixation

          # saccade stuff:
          #if(j==1){
          #  max_sent[j]<- sent[j]
          #} else{
          #  max_sent[j]<- max_sent[j-1]

          #  if(!is.na(max_sent[j])& !is.na(sent[j]) & sent[j]> max_sent[j]){
          #    max_sent[j]<- sent[j]
          #  }
          #}

          # maximum word:
          #if(j==1){
          #  max_word[j]<- abs(word[j])
          #  curr_sent[sent[j],2]<- abs(word[j])
          #} else{
          #  max_word[j]<- curr_sent[sent[j],2]
          #  if(!is.na(word[j])& abs(word[j])>curr_sent[sent[j],2]){
          #    max_word[j]<- abs(word[j])
          #    curr_sent[sent[j],2]<- abs(word[j]) # replace new max word
          #  }
          #}

          # Regression stuff:
          #if(!is.na(max_sent[j])& !is.na(sent[j]) & sent[j]< max_sent[j]){
          #  intersent_regr[j]<- 1
          #} else{
          #  intersent_regr[j]<- 0
          #}

          # intra-sentence regressions:
          #if(!is.na(word[j])& abs(word[j])<max_word[j]){
          #  intrasent_regr[j]<- 1
          #}else{
          #  intrasent_regr[j]<- 0
          #}


        } # end of k loop
      }

    } # end of j loop

  } # end of i loop

} # end of function
