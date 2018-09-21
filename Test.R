

rm(list= ls())

library(EMreading)

data<- SLpreproc(data_list= "C:/Users/Martin Vasilev/Documents/Test/",
                 ResX= 1024, ResY=768, maxtrial= 80)
save(data, file = "data.Rda")

dataN<- cleanData(data, silent = F)

#dataN<- cleanData(data, outlierMethod = "std", outlierCutoff = 3)
