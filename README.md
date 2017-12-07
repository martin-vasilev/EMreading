## Installation:

To install the package, please run the following code in R:

``` R
  if('devtools' %in% rownames(installed.packages())==FALSE){
    message(paste("Installing required package ", toupper('devtools'), "..."))
    install.packages('devtools')
    library(devtools)
  }else{
    library(devtools)
  }
install_github('martin-vasilev/paraFix')
```



**Please note that the script is still under development and has not been extensively tested.**
