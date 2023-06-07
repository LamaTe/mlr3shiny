# mlr3shiny: Machine Learning in Shiny with mlr3 <img src="man/figures/logo.png" align="right" width = "120" />

[![Build Status](https://app.travis-ci.com/LamaTe/mlr3shiny.svg?branch=master)](https://app.travis-ci.com/github/LamaTe/mlr3shiny)

This application provides the basic steps of a machine learning workflow from a graphical user interface built with Shiny. It uses the functionalities of the R-package [mlr3](https://mlr3.mlr-org.com).

Current functionalities of mlr3shiny are:
* Data import
* Creation of a task for supervised learning (regression, classification)
* Use of a set of algorithms as learners
* Training and evaluation of the generated models
* Benchmarking to compare several learners on a task simultaneously
* Prediction on new data using the trained learner
* Explain trained learner 

[Reference](https://mlr3.mlr-org.com](https://www.sciencedirect.com/science/article/pii/S2352711022001649).

## Installation

Install the package in R via CRAN:

``` r
install.packages(mlr3shiny)
```

Install the development version of the package in R from GitHub.
``` r
remotes::install_github("https://github.com/LamaTe/mlr3shiny.git")
```

## Example
Launch the application via:
``` r
mlr3shiny::launchMlr3Shiny()
```

## Usage Description
Navigate over the different steps of the workflow using the menu bar. The tabs are chronologically ordered. 
The question mark in the top-right corner provides more information on the functionalities and purpose of each section.
Start by importing a dataset. Then define a task (the problem to be solved) in the 'task' tab. Example tasks are already provided. Select different learners (algorithms) in the 'learner' tab and train and evaluate a model in 'train & evaluate'. 
Resampling strategies can be applied in a sub-section of 'train & evaluate'.
Alternatively, different learners can be compared in a benchmark.
Use the final model to make a prediction on new data in the 'predict' tab. 
An explanation of the final model from the predict tab can be made in the 'explain' tab. 

## References to Algorithms
* [linear/ logistic regression](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html)
* [decision tree](https://CRAN.R-project.org/package=rpart)
* [random forest](https://CRAN.R-project.org/package=ranger)
* [support vector machine](https://CRAN.R-project.org/package=e1071)
* [xgboost](https://CRAN.R-project.org/package=xgboost)
