# mlr3shiny: Machine Learning in Shiny with mlr3
This application let's you conduct the basic steps of a machine learning workflow from a grahpical user interface built with Shiny.  It uses the functionalities of the R-package [mlr3](https://mlr3.mlr-org.com).

Current functionalities of mlr3shiny are:
* Data import
* Creation of a task for supervised learning (regression, classification)
* Use of a set of algorithms as learners
* Training and evaluation of the generated models
* Benchmarks to compare several learners on a task simultaneously
* Prediction on new data using the trained learnder

## Installation
Install the package in R from GitHub.
```
remotes::install_github("https://github.com/LamaTe/mlr3shiny.git")
```
Launch the application via:
```
launchMlr3Shiny()
```

## Usage Description
Navigate over the different steps of the workflow using the menu bar. The tabs are chronologically ordered. 
The questionmark in the rop-right corner provides more information on the functionalities and purpose of each section.
Start by importing a dataset. Then define a task (the problem to be solved) in the 'task' tab. Example tasks are already provided. Select different learners (algorithms) in the 'learner' tab and train and evaluate a model in 'train & evaluate'. 
Resampling strategies can be applied in a sub-section of 'train & evaluate'.
Alternatively, different learners can be compared in a benchmark.
Use the final model to make a prediction on new data in the 'predict' tab. 
