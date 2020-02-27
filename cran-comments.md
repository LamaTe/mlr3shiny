## Resubmission
This is a resubmission. In this version I have:

* Written package names, software names etc. in single quotes in title and description field
* Changed the start of the description to one that doesn't start with 'This is an R package' or similar
* Added information about what kind of value launchMLR3Shiny returns
* There was no new code added to the directory R instead of inst since there is no new statistical functionality added but only functionality implemented from the package 'mlr3'. The functions on the server side work with reactive values which makes it hard to wrap them and export them to the R directory. To make up for the missing checks and executable examples in batch mode shinytest is used to verify the different functionalities of the application to the best extend.

## Test environments
* local manjaro 18.1.5, R 3.6.2
* ubuntu 14.05 (on travis-ci), R 3.6.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* New submission

  First time CRAN submission
