
## Installation:

To install the package, please run the following code in R:

``` R
  if('devtools' %in% rownames(installed.packages())==FALSE){
    install.packages('devtools')
    library(devtools)
  }else{
    library(devtools)
  }
install_github('martin-vasilev/readingET')
```
