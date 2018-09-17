
rm(list= ls())

library(EMreading)

data<- SLpreproc(data_list= "C:/Users/Martin Vasilev/Documents/Test/",
                 ResX= 1920, ResY=1080, maxtrial= 120)
save(data, file = "data.R")

dataN<- cleanData(data)
