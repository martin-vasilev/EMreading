
#' Extracts the calibration accuracy information from asc files (for data quality 
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
#' @return A data frame containing the calibration accuracy data
#'
#' @example
#' drift_data<- Drift(data_list= "D:/Data/subject1.asc")
#'
#' @include utility.R
#' 


Calibr<- function(data_list, keep_time_diff=0){
  
  
  get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
  
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
  
  df<- NULL
  for (i in 1:length(data)){ # for each subject:
    filename= data[i]
    file= readLines(data[i])
    
    if(i==1){
      type<- which(grepl(">>>>>>> CALIBRATION", file))
      type<- get_num(file[type[1]])
      
      message(type, "-point calibration used") 
    }
    
    text<- file[which(grepl("VALIDATE", file))]
    
    # parse message text:
    out <-  do.call( rbind, strsplit( text, '\t' ) )
    out<- out[, 2]
    out <-  do.call( rbind, strsplit(out, ' ' ) )
    
    # remove empty columns that can mess up parsing sometimes:
    out<- out[, colSums(out != "") != 0]
    
    time_stamp<- as.numeric(out[,1])
    eye<- out[, 6]
    
    offset<- as.numeric(out[,10])
    pos<- as.numeric(unlist(strsplit(out[,8], ',')))
    x_pos<- pos[c(TRUE, FALSE)]
    y_pos<- pos[c(FALSE, TRUE)]
    
    pix_offset<- as.numeric(unlist(strsplit(out[,12], ',')))
    x_offset<- pix_offset[c(TRUE, FALSE)]
    y_offset<- pix_offset[c(FALSE, TRUE)]
    sub= rep(i, length(time_stamp))
    
    df_temp<- try(data.frame(sub, time_stamp, eye, offset, x_pos, y_pos, x_offset, y_offset))
    try(assign('df_temp$filename', filename))
    
    
    ##### check for repeated calibrations (take last attempt):
    
    if(keep_time_diff>0){
      
      try(assign('df_temp$keep', 1)) 
      done= FALSE
      curr_step= type
      for(j in 1:(nrow(df_temp)/type-1)){
        t_diff<- (df_temp$time_stamp[curr_step+type]- df_temp$time_stamp[curr_step])/(60*1000) # in mins
        if(length(t_diff)<1){
          next
        }
        if(t_diff<keep_time_diff){
          df_temp$keep[(curr_step- type+1) :curr_step]= 0
        }
        curr_step= curr_step +type # increment
        # if(curr_step+type== nrow(df_temp)){
        #   done= TRUE
        }
      }
    
    df<- try(rbind(df, df_temp))
    
    try(cat(sprintf("Subject %i offset: mean: %.3f, SD: %.3f, range: %.3f - %.3f",
                i, mean(df_temp$offset), sd(df_temp$offset), range(df_temp$offset)[1],
                range(df_temp$offset)[2])))
    cat("\n")
    
  }
  
  
  return(df)
  
}
