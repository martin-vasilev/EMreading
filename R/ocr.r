
# Martin R. Vasilev, 2018

# tentative code for reading in image stimuli:

#library(magick)
#library(tesseract)

### functions:
#--------------------------------------------------------------------------#
numerize<- function(string){as.numeric(unlist(strsplit(string, ",")))}
#--------------------------------------------------------------------------#

image_file<-"C:/Users/Martin Vasilev/Documents/Oz/Oz/Dorothy/Dorothy2.bmp"

# read in image:
input <- magick::image_read(image_file)

# Recognize text image:
text <- tesseract::ocr(input)

# Get the pixel locations of words:
loc<- tesseract::ocr_data(input)

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
