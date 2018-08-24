
# Martin R. Vasilev, 2018

# tentative code for reading in image stimuli:

OCR<- function(image_file= "C:/Users/Martin Vasilev/Documents/Oz/Oz/Dorothy/Dorothy2.bmp",
               language= "English"){

  # Additional functions:
  numerize<- function(string){as.numeric(unlist(strsplit(string, ",")))}

  # default language is English
  # check if user defined another available language (gives better accuracy)
   allowedLangs<- c("Spanish", "Dutch", "Korean", "French", "Japanese")
   langCodes<- c("spa", "nld", "kor", "fra", "jpn")

   if(language!= "English" & is.element(language, allowedLangs)){ # is lang valid?
     whichLang<- langCodes[which(language== allowedLangs)]

      if(!is.element(whichLang, tesseract::tesseract_info()$available)){ # already downloaded?
        cat("downloading trained language data...")
        tesseract::tesseract_download(whichLang) # if not, download
        useLang<- tesseract::tesseract(whichLang) # then define
      }else{
        useLang<- tesseract::tesseract(whichLang) # if downloaded, directly define
      }

   }else{ # defaults to English:
      useLang<- tesseract::tesseract("eng")
   }

   ### Start OCR:
   # read in image:
   input <- magick::image_read(image_file)

   # Recognize text image:
   text <- tesseract::ocr(input, engine= useLang)

   # Get the pixel locations of words:
   loc<- tesseract::ocr_data(input , engine= useLang)

   # make coords in a matrix:
   coords<- as.data.frame(t(matrix(data = numerize(loc$bbox), nrow = 4)))
   colnames(coords)<- c("x1", "y1", "x2", "y2")
   coords$conf<- loc$confidence
   coords$word<- loc$word

   # I need to parse the text so that I know which words are line final;
   # I need to adjust y1 and y2 to be the same for all words on the line-
   # to do that, take min(y1) and max (y2). This will give us the max text
   # window on the line.

   # Think also about what to do with empty spaces:

   return(coords)
}


#lines<- unlist(strsplit(text, "\n"))
#if(lines[length(lines)]==""){
#  lines<- lines[1:(length(lines)-1)]
#}

letterize<- function(dat){

  coords<- NULL
  temp<- structure(list(char = structure(NA_integer_, .Label = character(0), class = "factor"),
                        letter = NA_character_, x1 = NA_real_, y1 = NA_real_, x2 = NA_real_,
                        y2 = NA_real_, space = NA_real_, sent = NA_real_, line = NA_integer_,
                        word = NA_real_, line_char = NA_integer_), .Names = c("char",
                        "letter", "x1", "y1", "x2", "y2", "space", "sent", "line", "word",
                        "line_char"), row.names = 1L, class = "data.frame")

  for(i in 1:nrow(data)){
    if(i!= nrow(data)){
      df<- temp[rep(seq_len(nrow(temp)), each=nchar(dat$word[i])+1),]
      df$letter<- c(unlist(strsplit(dat$word[i], '')), "")

      df$x1<- c(rep(dat$x1[i], nchar(dat$word[i])), NA)
      df$x2<- c(rep(dat$x2[i], nchar(dat$word[i])), NA)
      df$y1<- c(rep(dat$y1[i], nchar(dat$word[i])), NA)
      df$y2<- c(rep(dat$y2[i], nchar(dat$word[i])), NA)
    }else{
      df<- temp[rep(seq_len(nrow(temp)), each=nchar(dat$word[i])),]
      df$letter<- unlist(strsplit(dat$word[i], ''))

      df$x1<- rep(dat$x1[i], nchar(dat$word[i]))
      df$x2<- rep(dat$x2[i], nchar(dat$word[i]))
      df$y1<- rep(dat$y1[i], nchar(dat$word[i]))
      df$y2<- rep(dat$y2[i], nchar(dat$word[i]))
    }


    coords<- rbind(coords, df)
  }
  rownames(coords)<- seq(1, nrow(coords),1)
  coords$char<- 0:(nrow(coords)-1)

}
