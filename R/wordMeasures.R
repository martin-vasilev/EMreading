
#' Calculation of word-level fixation measures (FFD, SFD, GD, TVT, skipping, regressions, number of fixations) using raw fixation data.
#' 
#' @author Martin R. Vasilev
#' 
#' 
#' @param data Dataframe containing the raw fixation data that were extracted with the
#' EMreading package
#' 
#' @param multipleItems a logical indicating whether subject saw a given item more than once (FALSE by default). 
#' If it is set to TRUE, it will calculate a given item multiple times based on how many conditions it appears in.
#' 
#' @includeTimeStamps a logical indicating whether to include the start of fixation (SFIX) and end of fixation 
#' (EFIX) from the raw data. If set to TRUE, it will add these measures (default is FALSE)

wordMeasures<- function(data, multipleItems=FALSE, includeTimeStamps= FALSE){
  
  if(length(which(is.na(data$word)))== nrow(data)){
    stop("Only data with fixations mapped to the text stimuli can be processed!")
  }
  
  addBlinks= FALSE
  BlinkFixTypeNotMapped<- FALSE
  
  # check if prev_blink, blink, and after_blink columns exist:
  if("blink" %in% colnames(data) ){ # & "prev_blink" %in% colnames(data) & "after_blink" %in% colnames(data)
    if(sum(data$blink, na.rm = T)==0){ # + sum(data$prev_blink, na.rm = T) +sum(data$after_blink, na.rm = T)
      cat("Blinks appear to be already excluded! \n\n");
    }else{
      cat("There appears to be valid blink data! We will map blinks to individual words. \n\n");
      addBlinks= TRUE
    }
    
    regress_blinks<- which(!is.na(data$regress[which(data$blink==1)])) # data$prev_blink==1 | data$after_blink==1
    
    if(length(regress_blinks)<1){
      BlinkFixTypeNotMapped<- TRUE
      
      cat("Fixation type is not mapped for observations with blinks. Therefore, blinks can't be mapped in terms of 1st and 2nd pass reading. ");
      cat("Please note that, by default, blink fixation durations will also not be added to fixation duration measures for that word since it's assumed you will delete this word from analysis.\n")
      cat("If you need to change this, see settings in the pre-processing function.\n\n");
    }
    
  }else{
    cat("No information about blinks appears to be in the data! \n");
  }
  
  
  
  sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; o<- NULL; p<- NULL
  nitems<- NULL; n<- NULL; p1<- NULL; p2<- NULL; wordID<- NULL; word_line<- NULL
  dataN<- NULL; dataT<- NULL; q<- NULL; r<- NULL; sent<- NULL
  FFD<- NULL; SFD<- NULL; GD<-NULL; TVT<- NULL; line<- NULL
  nfix1<- NULL; nfix2<- NULL; nfixAll<- NULL; regress<- NULL
  
  cat("Processing data for subject... ");
  
  
  if(multipleItems){ # special case where an item is seen in more than 1 cond per subject
    for(i in 1:length(unique(data$sub))){ # for each subect..
      
      nitems<- unique(data$item[data$sub==i])# trials that participant saw
      nitems<- sort(nitems)
      cat(paste(i, " ", sep=""));
      
      for(j in 1: length(nitems)){ # for each item of subject i
        
        n<- subset(data, sub==i & item==nitems[j]) # subset data for subect i & item j
        conditions<- unique(n$cond)
        
        for(c in 1:length(conditions)){ # for each condition c...
          n2<- subset(n, cond== conditions[c]) # subset item for condition c
          o<- sort(unique(n2$sent))
          
          for(k in 1:length(o)){ # for each sentence...
            q<- subset(n2, sent==o[k])
            r<- sort(unique(q$word))
            
            for(l in 1:length(r)){ # for each word in sentence k
              word[l]<- r[l]
              sub[l]<- n2$sub[1]
              item[l]<- n2$item[1]
              seq[l]<- n2$seq[1]
              cond[l]<- n2$cond[1]
              sent[l]<- o[k]
              
              
              ### Refixation conditional:
              p<- subset(q, word==r[l])
              
              # add additional metrics:
              
              
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
              
              # probability of making a regression to word
              if(nfix2[l]==0){
                regress[l]<- 0
              }else{
                regress[l]<- 1
              }
              
              
            } # end of l
            
            if(is.na(word[1])){
              sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; p<- NULL; sent<- NULL
              p1<- NULL; p2<- NULL; q<- NULL; r<- NULL; wordID<- NULL
              FFD<- NULL; SFD<- NULL; GD<-NULL; TVT<- NULL
              nfix1<- NULL; nfix2<- NULL; nfixAll<- NULL; regress<- NULL
              
              next
            }
            
            dataT<- data.frame(sub, item, cond, seq, word, wordID, sent, FFD, SFD, GD, TVT,
                               nfix1, nfix2, nfixAll, regress)
            sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; p<- NULL; sent<- NULL
            p1<- NULL; p2<- NULL; q<- NULL; r<- NULL; wordID<- NULL
            FFD<- NULL; SFD<- NULL; GD<-NULL; TVT<- NULL; line<- NULL
            nfix1<- NULL; nfix2<- NULL; nfixAll<- NULL; regress<- NULL
            
            dataN<- rbind(dataN, dataT)
            
          } # end of k
          
        } # end of c
        
        
      } # end of j
      
    } # end of i
    
  }else{ # default
    
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
          
          
          ## TEMP!: fix first-pass for corrective saccades:
          
          if(max(n$line, na.rm = T)>1){
            check_next<- F
            RS_word<- NA
            
            for (z in 1:nrow(q)){
              
              if(!is.na(q$Rtn_sweep[z])){
                if(q$Rtn_sweep[z]==1){
                  check_next= T
                  RS_word<- q$word_line[z]
                }
              }
              
              
              if(check_next){
                
                if(!is.na(q$word_line[z]) & !is.na(q$regress[z])){
                  
                  if(q$word_line[z]<= RS_word){
                    
                    if(q$regress[z]==1){
                      q$regress[z]=0
                    }
                    
                    
                  }else{ # stop checking
                    check_next <- F
                    RS_word<- NA
                  }
                  
                } # if not NA..
                
                
              } # if check next
              
              
              
            } # z loop end
            
          } # end of check if there is more than 1 line


          
          
          
          # pre-alocate return sweeps as NAs (in case of single line):
          RS<- rep(NA, length(r))
          RS_type<- rep(NA, length(r))
          
          if(includeTimeStamps){
            SFIX<- rep(NA, length(r))
            EFIX_FFD<- rep(NA, length(r))
            EFIX_SFD<- rep(NA, length(r))
            EFIX_GD<- rep(NA, length(r))
            EFIX_TVT<- rep(NA, length(r))
          }
          
          if(addBlinks){
            blinks_1stPass<- rep(NA, length(r))
            blinks_2ndPass<- rep(NA, length(r))
            blinks<- rep(NA, length(r)) # if fixation type was not tracked (by user choice..)
          }
          
          
          for(l in 1:length(r)){ # for each word in sentence k
            word[l]<- r[l]
            sub[l]<- n$sub[1]
            item[l]<- n$item[1]
            seq[l]<- n$seq[1]
            cond[l]<- n$cond[1]
            sent[l]<- o[k]
            
            
            ### Refixation conditional:
            p<- subset(q, word==r[l])
            
            # add line, etc.:
            line[l]<- p$line[1]
            word_line[l]<- p$word_line[1]
            
            if(nrow(p)==0){
              FFD[l]<- NA
              SFD[l]<- NA
              GD[l]<- NA
              TVT[l]<- NA
              nfix1[l]<- 0
              nfix2[l]<- 0
              nfixAll[l]<- 0
              
            } else{
              
              # return-sweep stuff:
              if(max(n$line, na.rm = T)>1){
                if (!is.null(p$Rtn_sweep[1])){
                  
                  if(!is.na(p$Rtn_sweep[1])){
                    if(sum(p$Rtn_sweep)>0){
                      RS[l]<- 1
                      
                      # Return-sweep type:
                      type<- p$Rtn_sweep_type[which(!is.na(p$Rtn_sweep_type))]
                      if(length(type)>0){
                        RS_type[l]<- type
                      }
                      
                    }else{
                      RS[l]<- 0
                    }
                  }
                  
                }
              }
              

              
              
              # Split by fixation type:
              p1<- subset(p, regress==0) # first-pass fixations
              p2<- subset(p, regress==1) # second-pass fixations
              
              # if we have to include blinks...
              if(addBlinks){
                
                # first-pass blinks:
                sum_1st<- sum(p1$blink, na.rm = T) #+ sum(p1$prev_blink, na.rm = T) + sum(p1$after_blink, na.rm = T)
                if(sum_1st>0){
                  blinks_1stPass[l]<- 1 
                }else{
                  blinks_1stPass[l]<- 0
                }
                
                # second-pass blinks:
                sum_2nd<- sum(p2$blink, na.rm = T)# + sum(p2$prev_blink, na.rm = T) + sum(p2$after_blink, na.rm = T)
                if(sum_2nd>0){
                  blinks_2ndPass[l]<- 1 
                }else{
                  blinks_2ndPass[l]<- 0
                }
              
              # if blink fixations are not mapped in terms for 1st and second pass-reading...  
              if(BlinkFixTypeNotMapped){
               
                sum_all<- sum(p$blink, na.rm = T) #+ sum(p$prev_blink, na.rm = T) + sum(p$after_blink, na.rm = T)
                
                if(sum_all>0){
                  blinks[l]<- 1 
                }else{
                  blinks[l]<- 0
                }
                
                
                 
              }
                  
                
                  
              }# if addBlinks
              
              
              
              
              if(nrow(p1)==0){ # no first-pass fixations
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
              
              # add time stamps
              if(includeTimeStamps){
                SFIX[l]<- p$SFIX[1]
                
                EFIX_TVT[l]<- p$EFIX[nrow(p)]
                
                if(nrow(p1)>0){
                  EFIX_FFD[l]<- p1$EFIX[1]
                  EFIX_GD[l]<- p1$EFIX[nrow(p1)]
                  
                  if(nrow(p1)==1){
                    EFIX_SFD[l]<- p1$EFIX
                  }
                  
                  if(nrow(p1)>1){
                    EFIX_GD[l]<- p1$EFIX[nrow(p1)]
                  }
                }
                
              }
                
            }
            
            # probability of making a regression to word
            if(nfix2[l]==0){
              regress[l]<- 0
            }else{
              regress[l]<- 1
            }
            
            
          } # end of l
          
          if(is.na(word[1])){
            sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; p<- NULL; sent<- NULL
            p1<- NULL; p2<- NULL; q<- NULL; r<- NULL; wordID<- NULL; word_line<- NULL
            FFD<- NULL; SFD<- NULL; GD<-NULL; TVT<- NULL; line<- NULL
            nfix1<- NULL; nfix2<- NULL; nfixAll<- NULL; regress <- NULL
            
            next
          }
          
          dataT<- data.frame(sub, item, cond, seq, word, word_line, wordID, sent, line, FFD, SFD, GD, TVT,
                             nfix1, nfix2, nfixAll, regress, RS, RS_type)
          
          if(includeTimeStamps){
            dataT$SFIX<- SFIX
            dataT$EFIX_FFD<- EFIX_FFD
            dataT$EFIX_SFD<- EFIX_SFD
            dataT$EFIX_GD<- EFIX_GD
            dataT$EFIX_TVT<- EFIX_TVT
          }
          
          if(addBlinks){
            
            if(BlinkFixTypeNotMapped){
              dataT$blinks<- blinks
            }else{
              dataT$blinks_1stPass<- blinks_1stPass
              dataT$blinks_2ndPass <- blinks_2ndPass
            }
            
          }
          
          sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; p<- NULL; sent<- NULL
          p1<- NULL; p2<- NULL; q<- NULL; r<- NULL; wordID<- NULL; word_line<- NULL
          FFD<- NULL; SFD<- NULL; GD<-NULL; TVT<- NULL; line<- NULL
          nfix1<- NULL; nfix2<- NULL; nfixAll<- NULL; regress<- NULL
          
          dataN<- rbind(dataN, dataT)
          
        } # end of k
        
      } # end of j
      
    } # end of i
    
  } # end of if multiple item
  
  if(max(dataN$sent)==1){
    dataN$sent<- NULL # remove sent number if there is only one sentence...
  }
  
  #### 
  # Add skipping measures:
  # First-pass skipping is when nfix1==0
  dataN$skip_1st<- ifelse(dataN$nfix1==0, 1, 0)
  dataN$skip_total<- ifelse(dataN$nfixAll==0, 1, 0)
  
  

  return(dataN)
}
