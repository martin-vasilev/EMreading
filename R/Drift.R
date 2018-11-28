
#' Extracts the drift check information from asc files (for data quality 
#' monitoring, etc.)
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
#' @return A data frame containing the drift check data
#'
#' @example
#' drift_data<- Drift(data_list= "D:/Data/subject1.asc")
#'
#' @include utility.R

Drift<- function(data_list){
  
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
  
  message("Note that since Eyelink 1000 only drift check is performed,
        meaning that no correction to the calibration is made.")
  
  df<- NULL
  for (i in 1:length(data)){ # for each subject:
    
    filename= data[i]
    file<- readLines(data[i]) # load file
    
    line<- which(grepl('DRIFTCORRECT', file))
    text<- file[line]
    
    # remove aborted drift checks
    whichAbort<- grep("ABORTED", text)
    nAborted<- length(text[whichAbort])
    if(nAborted>0){
      text<- text[-whichAbort]
    }
    
    # Remove repeated drift checks:
    whichRepeat<- grep("REPEATING", text)
    nRepeated<- length(text[whichRepeat])
    if(nRepeated>0){
      text<- text[-whichRepeat]
    }
    
    # parse message text:
    out <-  do.call( rbind, strsplit( text, '\t' ) )
    out<- out[, 2]
    out <-  do.call( rbind, strsplit(out, ' ' ) )
    
    time_stamp<- as.numeric(out[,1])
    eye<- out[, 4]
    offset<- as.numeric(out[,9])
    pos<- as.numeric(unlist(strsplit(out[,6], ',')))
    x_pos<- pos[c(TRUE, FALSE)]
    y_pos<- pos[c(FALSE, TRUE)]
    
    pix_offset<- as.numeric(unlist(strsplit(out[,12], ',')))
    x_offset<- pix_offset[c(TRUE, FALSE)]
    y_offset<- pix_offset[c(FALSE, TRUE)]
    sub= rep(i, length(time_stamp))
    
    df_temp<- data.frame(sub, time_stamp, eye, offset, x_pos, y_pos, x_offset, y_offset)
    df_temp$filename= filename
    
    df<- rbind(df, df_temp)
    
    cat(sprintf("Subject %i offset: mean: %.3f, SD: %.3f, range: %.3f - %.3f (%i aborted, %i repeated)",
               i, mean(df_temp$offset), sd(df_temp$offset), range(df_temp$offset)[1],
               range(df_temp$offset)[2], nAborted, nRepeated))
    cat("\n")
  }
  
  return(df)
}
