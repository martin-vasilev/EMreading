
#' Extracts the Validation information from asc files (for data quality 
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
#' @return A data frame containing the Validation data
#'
#' @example
#' drift_data<- Validate(data_list= "D:/Data/subject1.asc")
#'
#' @include utility.R

Validate<- function(data_list){
  
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
        meaning that no correction to the calibration is made.\n")
  
  df<- NULL
  for (i in 1:length(data)){ # for each subject:
    
    filename= data[i]
    file<- readLines(data[i]) # load file
    
    line<- which(grepl('VALIDATE', file))
    text<- file[line]
    
    # parse message text:
    out <-  do.call( rbind, strsplit( text, '\t' ) )
    out<- out[, 2]
    out <-  do.call( rbind, strsplit(out, ' ' ) )
    
    # remove empty columns that can mess up parsing sometimes:
    out<- out[, colSums(out != "") != 0]
    
    time_stamp<- as.numeric(out[,1])
    eye<- out[, 3]
    target_num<- as.numeric(out[, 5]) 
    
    deg<- as.numeric(out[,10])
    
    #offset<- as.numeric(out[,8])
    pos<- as.numeric(unlist(strsplit(out[,8], ',')))
    x_pos<- pos[c(TRUE, FALSE)]
    y_pos<- pos[c(FALSE, TRUE)]
    
    pix_offset<- as.numeric(unlist(strsplit(out[,12], ',')))
    x_offset<- pix_offset[c(TRUE, FALSE)]
    y_offset<- pix_offset[c(FALSE, TRUE)]
    sub= rep(i, length(time_stamp))
    
    df_temp<- try(data.frame(sub, time_stamp, eye, target_num, x_pos, y_pos, deg, x_offset, y_offset))
    try(assign('df_temp$filename', filename))
    
    df<- try(rbind(df, df_temp))
    
    try(cat(sprintf("Subject %i offset: mean: %.3f, SD: %.3f, range: %.3f - %.3f degrees",
               i, mean(df_temp$deg), sd(df_temp$deg), range(df_temp$deg)[1],
               range(df_temp$deg)[2])))
    cat("\n")
  }
  
  return(df)
}
