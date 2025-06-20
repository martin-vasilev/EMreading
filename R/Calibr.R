
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
#'@param keepLast Keep only the last calibration before recording new samples (TRUE= yes)
#'
#' @return A data frame containing the calibration accuracy data
#'
#' @example
#' drift_data<- Drift(data_list= "D:/Data/subject1.asc")
#'
#' @include utility.R
#' 


Calibr<- function(data_list, keepLast=TRUE){
  
  
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
    
    df_temp<- NULL
    
    for(j in 1:length(text)){
      
      # subject:
      subject<- i
      
      # time flag:
      string<- text[j]
      
      time_stamp<- get_num(unlist(strsplit(string, ' '))[1])
      
      if(length(time_stamp)<1){
        time_stamp<- NA
      }
      
      # filename: 
      flnm<- filename
      
      # eye:
      eye <- unlist(regmatches(string, gregexpr("\\b(LEFT|RIGHT)\\b", string)))
      
      if(length(eye)<1){
        eye<- NA
      }
      
      # calibration point:
      point<- unlist(strsplit(x = string, 'POINT '))[2]
      point<- as.numeric(unlist(strsplit(x = point, ' '))[1])
      
      if(length(point)<1){
        point<- NA
      }
      
      
      # offset: 
      offset_deg<- unlist(strsplit(x = string, 'OFFSET '))[2]
      offset_deg<- as.numeric(unlist(strsplit(x = offset_deg, 'deg.'))[1])
      
      if(length(offset_deg)<1){
        offset_deg<- NA
      }
      
      try(t<- data.frame(subject, flnm, time_stamp, eye, point, offset_deg))
      
      df_temp<- rbind(df_temp, t)
    }
    
  
    ##### check for repeated calibrations (take last attempt):
    
    if(keepLast){
      
      ### between each calibration attempt, check if there any new samples. 
      # If there were none, it means it wasn't the last calibration attempt
      # in the sequence
      new_df<- NULL
      
      # find where new calibrations start:
      df_temp$diff<- c(0, diff(df_temp$time_stamp))
      
      # row numbers with new calibration start:
      which_rows<- c(1, which(df_temp$diff>0))
      
      
      
      for(r in 1:length(which_rows)){
        if(r<length(which_rows)){
          
          start<- which(grepl(as.character(df_temp$time_stamp[which_rows[r]]), file))
          if(length(start)>1){
            start= start[length(start)] # keep only last row in sequence
          }
          
          # find start of NEXT calibration event:
          end<- which(grepl(as.character(df_temp$time_stamp[which_rows[r+1]]), file))
          
          if(length(end)>1){
            end<- end[1]
          }
          
          snippet<- file[start:end]
          
          check_flag<- which(grepl('START', snippet))
          
          if(length(check_flag)>0){
            start_time= get_num(unlist(strsplit(file[start], ' '))[1])
            new_df<- rbind(new_df, df_temp[which(df_temp$time_stamp==start_time),])
          }
          
        }else{
          
          start<- which(grepl(as.character(df_temp$time_stamp[which_rows[r]]), file))
          if(length(start)>1){
            start= start[length(start)] # keep only last row in sequence
          }
          
          start_time= get_num(unlist(strsplit(file[start], ' '))[1])
          
          new_df<- rbind(new_df, df_temp[which(df_temp$time_stamp==start_time),])
          
        }
        
        
      }
      
      df_temp= new_df 
    }
    

    
    # if(keep_time_diff>0){
    #   
    #   try(assign('df_temp$keep', 1)) 
    #   done= FALSE
    #   curr_step= type
    #   for(j in 1:(nrow(df_temp)/type-1)){
    #     t_diff<- (df_temp$time_stamp[curr_step+type]- df_temp$time_stamp[curr_step])/(60*1000) # in mins
    #     if(length(t_diff)<1){
    #       next
    #     }
    #     if(t_diff<keep_time_diff){
    #       df_temp$keep[(curr_step- type+1) :curr_step]= 0
    #     }
    #     curr_step= curr_step +type # increment
    #     # if(curr_step+type== nrow(df_temp)){
    #     #   done= TRUE
    #     }
    #   }
    
    df<- try(rbind(df, df_temp))
    
    try(cat(sprintf("Subject %i offset: mean: %.3f, SD: %.3f, range: %.3f - %.3f",
                i, mean(df_temp$offset), sd(df_temp$offset), range(df_temp$offset)[1],
                range(df_temp$offset)[2])))
    cat("\n")
    
  }
  
  
  return(df)
  
}
