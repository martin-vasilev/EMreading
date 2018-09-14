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

### Pre-processing of single-line reading experiments recorded with Eyetrack

To pre-process the data, simply use the `SLpreproc()` function of the package. You will need to provide some basic information, such as directory containing the data files and some details about the experiment: e.g.,

```
# preprocess data
data<- SLpreproc(data_list= "C:/Users/Martin Vasilev/My Data", ResX= 1920, ResY=1080, maxtrial= 120)
```

This will give you a data frame containing all fixations in addition to most variables that you will need for later analysis.
To perform a basic clean-up of the data, you can use:

```
dataN<- cleanData(data)
```
So far, this function removes blinks from the data, outlier fixation durations, as well as very short fixations (all can be modified by user input). The function also reports statistics of % fixation removed from each category for easy reporting.
