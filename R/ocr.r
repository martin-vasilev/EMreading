
# Martin R. Vasilev, 2018

# tentative code for reading in image stimuli:

#library(magick)
#library(tesseract)

### functions:

### OCR<- function(image_file, language= "English"){

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

#}

#--------------------------------------------------------------------------#
numerize<- function(string){as.numeric(unlist(strsplit(string, ",")))}
#--------------------------------------------------------------------------#

image_file<-"C:/Users/Martin Vasilev/Documents/Oz/Oz/Dorothy/Dorothy2.bmp"

# read in image:
input <- magick::image_read(image_file)

# Recognize text image:
text <- tesseract::ocr(input) #, engine= useLang

# Get the pixel locations of words:
loc<- tesseract::ocr_data(input) #, engine= useLang

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

lines<- unlist(strsplit(text, "\n"))
if(lines[length(lines)]==""){
  lines<- lines[1:(length(lines)-1)]
}

cat(text)
image<- readImage(image_file)
