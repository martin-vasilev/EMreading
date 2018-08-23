
# Martin R. Vasilev, 2018

# tentative code for reading in image stimuli:

#library(magick)
#library(tesseract)

image_file<-"C:/Users/Martin Vasilev/Documents/Oz/Oz/Dorothy/Dorothy2.bmp"

input <- magick::image_read(image_file)

text <- input %>%tesseract::ocr()

lines<- unlist(strsplit(text, "\n"))
if(lines[length(lines)]==""){
  lines<- lines[1:(length(lines)-1)]
}

cat(text)
