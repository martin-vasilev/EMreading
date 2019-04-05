#' Pad lines for EyeDoctor processing 
#'
#' This function pads text lines in the raw asc files to enable processing of fixations that are just to the left or to the
#' right text margin. Please note that if lines are not padded, EyeDoctor maps all fixations outside the text area as -1, which 
#' makes it impossible to map them to the line area on which they occured. 
#'
#' @author Martin R. Vasilev
#'
#' @param data_dir A directory containing asc data files to be processed
#'
#' @param paddingSize How many characters to pad to the left and to the right of the line margin.
#'#'
#' @return Nothing; the padded asc files are directly saved to the input directory 
#'
#' @example
#' EyeDoctor_PadLines(data_dir= "D:/myData/", paddingSize= 5)
#' 
#' @include utility.R
#' 

EyeDoctor_PadLines<- function(data_dir= NULL, paddingSize= 5){

  options(scipen=999)
  
  # check if user provided data dir:
  if(length(data_dir)==0){
    data_dir= file.choose() # make them chose a file
    message("To process multiple files, please specify a directory in 'data_dir'")
  }

  # Get data file names:
  dataASC<- get_files(data_dir)
  
  for (i in 1:length(dataASC)){ # for each subject..
  
    cat(sprintf("Processing subject %i", i)); cat("\n")
    cat(sprintf("Loading data %s ...", dataASC[i]));
    filename= dataASC[i] #strsplit(data[i], "\\")
    
    file<- readLines(dataASC[i]) # load asc file
    
    # Open file conection for writting the padded file:
    newFileName<- paste(substr(filename, 1, nchar(filename)-4), "_padded.asc", sep= "")
    #fileOut <- textConnection(newFileName, "w")
    #writeLines(newFile, newFileName)
    
    cat(" Done"); cat("\n")
    
    newFile<- NULL
    count=1
    
    # get the necessary flags:
    nextFlag<- which(grepl('TRIALID', file))
    stamps<- file[nextFlag] # get trial stamps
    trials<- substr(stamps, unlist(gregexpr(pattern =' ',stamps[1]))[2]+1, nchar(stamps)) # extract ID flag
    trials<- gsub(" ", "", trials) # clean empty spaces
    E<- substr(trials, 1, 1) # find experimental trials
    
    nextFlag<- nextFlag[which(E=="E")] # subset only trials we need
    
    cat("Trial... ")
    
    for(j in 1:length(nextFlag)){
      
      trialDone= FALSE
      newFile<- c(newFile, file[count:nextFlag[j]])
      #writeLines(file[count:nextFlag[j]], fileOut) #write to file
      
      count<- count + (nextFlag[j]-count)+1 #length(newFile)+1
      #count<- nextFlag[j]+1
      lastY= 0
      y1= 0
      
      nextX<- 0
      
      FirstLine= TRUE
      lastString= NULL
      TextStarted= FALSE
      
      while(!trialDone){
        
        string= file[count]
        string2= unlist(strsplit(string, " "))
             
        flag= string2[2] # 1 is always message stamp
        
        if(!is.na(flag)){
          if(flag== "REGION"){
            x1= as.numeric(string2[length(string2)-3])
            y1= as.numeric(string2[length(string2)-2])
            x2= as.numeric(string2[length(string2)-1])
            y2= as.numeric(string2[length(string2)])
            
            if(lastY < y1){ # triger only when moving to next line:
              padString= NULL
              padStringEnd= NULL
              
              for(k in 1:paddingSize){
                if(k==1){
                  cx2= x1 # current x2 (left margin)
                  cx1= x1- (x2-x1)
                  
                  string3<- string2
                  string3[6]<- " "
                  string3[7]<- toString(cx1)
                  string3[9]<- toString(cx2)
                  
                  padString[k]<- paste(string3, collapse = " ")
                  
                  #padString[k]<- paste(string2[1], string2[2], string2[3], "0", string2[5], "~", 
                   #                    toString(cx1), toString(y1), toString(cx2), toString(y1))
                  
                  if(!FirstLine){
                    string2End= unlist(strsplit(lastString, " "))
                    
                    string3End<- string2End
                    string3End[6]<- " "
                    string3End[7]<- toString(as.numeric(string2End[9])- (as.numeric(string2End[9])-as.numeric(string2End[7])))
                    string3End[9]<- toString(as.numeric(string2End[9])) #+ (as.numeric(string2End[9])-as.numeric(string2End[7])))
                    
                    padStringEnd[k]<- paste(string3End, collapse = " ")
                    
                    nextX<- as.numeric(string3End[9])
                  }
                  
                }else{
                  cx2= cx1
                  cx1= cx2- (x2-x1)
                  
                  string3<- string2
                  string3[6]<- " "
                  string3[7]<- toString(cx1)
                  string3[9]<- toString(cx2)
                  
                  padString[k]<- paste(string3, collapse = " ")
                  #padString[k]<- paste(string2[1], string2[2], string2[3], "0", string2[5], "~", 
                  #                     toString(cx1), toString(y1), toString(cx2), toString(y1))
                }
                padString<- padString[paddingSize:1] # reverse order
                
                
                if(!FirstLine){
                  string3End[7]<- toString(nextX)
                  string3End[9]<- toString(nextX + (as.numeric(string2End[9])-as.numeric(string2End[7])))
                  padStringEnd[k]<- paste(string3End, collapse = " ")
                  nextX<- nextX+ (as.numeric(string2End[9])-as.numeric(string2End[7]))
                }
                
                
              } # end of k (padding n)
              
              # add padded string to new asc file:
              if(!FirstLine){
                newFile<- c(newFile, padStringEnd, padString, file[count])
                #writeLines(padString, fileOut)
                padString<- NULL # reset
                padStringEnd<- NULL # reset
              }else{
                newFile<- c(newFile, padString, file[count])
                #writeLines(padString, fileOut)
                padString<- NULL # reset 
              }
              
              
              # turn on FirsLine permanently (i.e., pad also at the end from now on):
              FirstLine= FALSE
              
            }else{
              newFile<- c(newFile, file[count])
              #writeLines(file[count], fileOut)
            }
            
            lastY<-  y1
            
            
            
          }else{
            newFile<- c(newFile, file[count])
            #writeLines(file[count], fileOut)
          }
        }else{
          newFile<- c(newFile, file[count])
          #writeLines(file[count], fileOut)
        }
        
        #### check if it is the last line; if so, padd the end as well:
        FlagN1= unlist(strsplit(file[count+1], " "))[2] # Flag of count+1
        
        if(!is.na(FlagN1)){
          if(FlagN1== "DELAY" | FlagN1== "REGION"){ # safe check to ensure we monitor only after we started parsin text coords..
            TextStarted<- TRUE
          }
        }
        
        
        if(TextStarted & !is.na(FlagN1)){
          if(!is.element(FlagN1, c("DELAY", "REGION"))){ # print end of text padding
            EndofTextpadding<- NULL
            
            for(k in 1:paddingSize){
              if(k==1){
                if(flag== "DELAY"){
                  endString= unlist(strsplit(file[count-1], " "))
                  
                  if(length(endString)>10){
                    endString<- endString[-7]
                  }
                  
                }else{
                  endString= unlist(strsplit(file[count], " "))
                  warning("I shouldn't be here!")
                }
                
                
                endString2<- endString
                endString2[6]<- " "
                endString2[7]<- toString(as.numeric(endString[9])) 
                endString2[9]<- toString(as.numeric(endString[9])+ (as.numeric(endString[9])-as.numeric(endString[7])))# #+ (as.numeric(string2End[9])-as.numeric(string2End[7])))
                
                EndofTextpadding[k]<- paste(endString2, collapse = " ")
                
                nextX<- as.numeric(endString2[9])
              }else{
                endString2<- endString
                
                endString2[6]<- " "
                endString2[7]<- nextX
                endString2[9]<- toString(nextX+ (as.numeric(endString[9])-as.numeric(endString[7])))
                
                EndofTextpadding[k]<- paste(endString2, collapse = " ")
                
                nextX= nextX+ (as.numeric(endString[9])-as.numeric(endString[7]))
                
              }
            } # end of k
            
            # add padding to new file:
            newFile<- c(newFile, EndofTextpadding)
            TextStarted= FALSE # turn off to stop printing flags
          }
        }
        
        
        
        if(length(string2)>0){
          trialDone= unlist(strsplit(string2[1], "\t"))[1]== "START" # data collection has started
        }
        
        count= count+1
        lastString= file[count-2]
      
      } # end of trialDone
     
      cat(toString(j)); cat(" ")
       
    } #end of j
    
    # add last of asc file after end of last trial:
    newFile<- c(newFile, file[count:length(file)])
    writeLines(newFile, newFileName) # save file to asc
    
    
    cat("\n DONE \n \n");
  } # end of i (subject loop)
  
  cat("\n \n All Done!");
  
}