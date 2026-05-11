
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
Frequency <- function(data, database = "SUBTLEX-UK", PoS = FALSE) {
  
  message("This function requires internet connection to download the requested database.")
  message("Attempting to load frequency database...")
  
  if (!"wordID" %in% names(data)) {
    stop("wordID column not found in dataframe!")
  }
  
  if (database == "SUBTLEX-UK") {
    db <- read.delim(
      url("https://github.com/martin-vasilev/R_scripts/raw/master/SUBTLEX-UK.csv"),
      sep = ";"
    )
    word_loc <- 1
    freq_loc <- 2
    zipf_loc <- 6
    PoS_loc <- 16
  } else if (database == "SUBTLEX-US") {
    db <- read.delim(
      url("https://github.com/martin-vasilev/R_scripts/raw/master/SUBTLEX-US.csv"),
      sep = ";"
    )
    word_loc <- 1
    freq_loc <- 2
    zipf_loc <- 15
    PoS_loc <- 16
  } else {
    stop("database must be either 'SUBTLEX-UK' or 'SUBTLEX-US'")
  }
  
  message("Mapping frequency to database...")
  
  # Convert words to character
  data$wordID <- as.character(data$wordID)
  db_word <- as.character(db[[word_loc]])
  
  # Create fallback versions of the words
  word_exact <- data$wordID
  word_lower <- tolower(word_exact)
  word_clean <- gsub("[[:punct:] ]+", "", word_lower)
  
  # Match in stages: exact -> lowercase -> punctuation-stripped
  loc <- match(word_exact, db_word)
  
  missing <- is.na(loc)
  loc[missing] <- match(word_lower[missing], db_word)
  
  missing <- is.na(loc)
  loc[missing] <- match(word_clean[missing], db_word)
  
  # Add results
  data$freq <- NA
  data$zipf <- NA
  
  matched <- !is.na(loc)
  data$freq[matched] <- db[[freq_loc]][loc[matched]]
  data$zipf[matched] <- db[[zipf_loc]][loc[matched]]
  
  if (PoS) {
    if (is.na(PoS_loc)) {
      warning("PoS information is not available for this database.")
      data$PoS <- NA
    } else {
      data$PoS <- NA
      data$PoS[matched] <- db[[PoS_loc]][loc[matched]]
    }
  }
  
  return(data)
}