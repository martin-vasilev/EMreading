
#' Calculation of word-level fixation measures (FFD, SFD, GD, TVT) using raw fixation data.
#' 
#' @author Martin R. Vasilev
#' 
#' 
#' @param data Dataframe containing the raw fixation data that were extracted with the
#' EMreading package

wordMeasures<- function(data){
  
  if(is.na(data$word[1])){
    stop("Only data with fixations mapped to the text stimuli can be processed!")
  }
  
  sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; o<- NULL; p<- NULL
  nitems<- NULL; n<- NULL; p1<- NULL; p2<- NULL; wordID<- NULL
  dataN<- NULL; dataT<- NULL; q<- NULL; r<- NULL; sent<- NULL
  FFD<- NULL; SFD<- NULL; GD<-NULL; TVT<- NULL;
  nfix1<- NULL; nfix2<- NULL; nfixAll<- NULL
  
  cat("Processing data for subject... ");
  
  for(i in 1:length(unique(data$sub))){ # for each subect..
    
    nitems<- unique(data$item[data$sub==i])# trials that participant saw
    nitems<- sort(nitems)
    cat(paste(i, " ", sep=""));
    
    for(j in 1: length(nitems)){ # for each item of subject i
      
      n<- subset(data, sub==i & item==nitems[j]) # subset data for subect i & item j
      o<- sort(unique(n$sent))
      
      for(k in 1:length(o)){ # for each sentence...
        q<- subset(n, sent==o[k])
        r<- sort(unique(q$word))
        
        for(l in 1:length(r)){ # for each word in sentence k
          word[l]<- r[l]
          sub[l]<- n$sub[1]
          item[l]<- n$item[1]
          seq[l]<- n$seq[1]
          cond[l]<- n$cond[1]
          sent[l]<- o[k]
          
          
          ### Refixation conditional:
          p<- subset(q, word==r[l])
          
          if(nrow(p)==0){
            FFD[l]<- NA
            SFD[l]<- NA
            GD[l]<- NA
            TVT[l]<- NA
            nfix1[l]<- 0
            nfix2[l]<- 0
            nfixAll[l]<- 0
          } else{
            # first-pass fixations:
            p1<- subset(p, regress==0)
            p2<- subset(p, regress==1)
            
            if(nrow(p1)==0){
              FFD[l]<- NA
              SFD[l]<- NA
              GD[l]<- NA
            }
            
            if(nrow(p1)==1){
              FFD[l]<- p1$fix_dur
              SFD[l]<- p1$fix_dur
              GD[l]<- p1$fix_dur
              #  TVT[l]<- p1$fix_dur
            }
            
            if(nrow(p1)>1){
              FFD[l]<- p1$fix_dur[1]
              SFD[l]<-NA
              GD[l]<- sum(p1$fix_dur)
              #  TVT[l]<- sum(p1$fix_dur)
            }
            
            
            # second-pass fixations:
            #p2<- subset(p, intrasent_regr==1 | intersent_regr==1)
            TVT[l]<- sum(p$fix_dur)
            
            nfix1[l]<- nrow(p1)
            nfix2[l]<- nrow(p2)
            nfixAll[l]<- nrow(p1)+ nrow(p2)
            
            # Add word ID:
            wordID[l]<- as.character(p$wordID[1])
          }
          
          
        } # end of l
        
        dataT<- data.frame(sub, item, seq, word, wordID, sent, cond, FFD, SFD, GD, TVT,
                           nfix1, nfix2, nfixAll)
        sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; p<- NULL; sent<- NULL
        p1<- NULL; p2<- NULL; q<- NULL; r<- NULL; wordID<- NULL
        FFD<- NULL; SFD<- NULL; GD<-NULL; TVT<- NULL
        nfix1<- NULL; nfix2<- NULL; nfixAll<- NULL
        
        dataN<- rbind(dataN, dataT)
        
      } # end of k
      
    } # end of j
    
  } # end of i
  
  return(dataN)
}

