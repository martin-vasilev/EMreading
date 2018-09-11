This is an R package that performs automatic pre-processing and mapping of fixations to text stimuli on the screen. The current implementation of this package only works with reading experiments recorded with the Eyetrack software (http://blogs.umass.edu/eyelab/software/). However, support for other types of software packages and experimental scripts will be added very soon. The goal is to create a general and easy-to-use tool that can perform data pre-processing with just a few commands by using the raw .asc data. For users unfamiliar with R, a web interface with Rshiny will also be created. Currently, most of the development is focused on working with single line reading studies. However, support for multiple-line reading data is also on the way.

I'm doing my best to debug the script and to test it rigouously. Nevertheless, bugs can always slip through. If you notice a problem or want any new functionality to be added, please email me at mvasilev@bournemouth.ac.uk or open an issue in the repository. Also, if the package doesn't work with your software/ equipment, please let me know and I would do my best to add support for it.

## Installation:

To install the package, please run the following code in R:

``` R
if('devtools' %in% rownames(installed.packages())==FALSE){
    install.packages('devtools')
    library(devtools)
  }else{
    library(devtools)
  }
install_github('martin-vasilev/EMreading')
```

## Current functionality:

