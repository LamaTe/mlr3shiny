library(shiny)
library(mlr3)
library(mlr3learners)
library(DT)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(data.table)
library(readxl)
library(stringr)
library(plyr)
library(purrr)


userhelp <- list(Data = c(paste("This app let's you conduct the basic steps of a machine learning workflow using your own data.",
                                "You can navigate over the different sections by clicking on each tab. They are chronologically ordered.",
                                "To get more information on the functionalities and purpose of each section, click on the questionmark in the rop-right corner.",
                                "Start by importing a dataset.",
                                "Then define a task (the problem to be solved) in the 'task' tab. Example tasks are already provided.",
                                "Select different learners (algorithms) in the 'learner' tab and train and evaluate a model in 'train & evaluate'.",
                                "Resampling strategies can be applied in a sub-section of 'train & evaluate'.",
                                "Alternatively, different learners can be compared in a benchmark.",
                                "Use the final model to make a prediction on new data in the 'predict' tab. ",
                                sep = " "
                          ),
                          paste("Here, you can import a dataset that you want to train your machine learning model on.",
                                "On the right-hand side an overview will be given of its composition.",
                                "The accepted file formats are .txt, .csv and .xlsx'.",
                                "Ensure that a header row is present in the dataset or else deactivate the checkbox 'Header'.",
                                sep = " ")),
                 "Task Creation" = c("A training dataset must be imported before using it as data backend for a task."),
                 "Task Creation Target" = c(paste("The target variable needs to be of type factor (categorical) or numeric (a number).",
                                                  "If unsure you can have a look at the training data in the DATA tab.")),
                 "Features Dropped" = c(paste("Your dataset contains unsupported features of type 'POSIXct' (Date and Time),",
                                              "'complex' (a complex number) or 'Date'.",
                                              "These features will be disabled and not used to train your model.")),
                 Task = c(paste("A task is an object to store the training dataset as backend and additional information about it as meta information.",
                                "These are needed due to the particular requirements of a machine-learning problem and e.g. include the target variable and specific roles for other variables.",
                                "Moreover, the meta information store the task type such as a classification or a regression.",
                                "If the target variable is of type numeric, the task will be of type regression. If the target is a factor,",
                                "the task is a classification and can predict either a target with two levels (two class) or a target with multiple classes (multiclass).", sep = " "),
                          paste("Two examplary tasks are provided. Otherwise, select the imported training data and set a target name and a task ID.",
                                "On the right-hand side you can drop features, meaning disregarding a variable from the dataset and thereby changing the view of the task.",
                                "This is useful for variables that do not provide an information gain such as an ID or a feature with only one level over all observations.",
                                "To inspect the data, return to the 'Data' tab.", sep = " ")),

                 Learner = c(paste("Different machine-learning algorithms can be selected via 'learners' which",
                                   "are objects that provide an interface to the actual algorithm and store meta information. They are depicted on the right-hand side",
                                   "under the sections 'overview' and 'parameters' and include necessary settings such as the predict type,",
                                   "the current hyperparameter values and other properties. Predict types can be response or probability for",
                                   "the target of a classification task and response or standard error for a regression task, depending on the algorithm.",
                                   sep = " "),
                             paste("Currently, one can select the algorithms: decision tree, random forest, support vector machine, logistic regression and",
                                   "linear regression. While logistic reg. is used for a classification task and linear reg. for a regression task, the others",
                                   "are available for both.", "Each learner has hyperparameters of wich an extract is available under 'parameter settings'",
                                   "to adjust the learner during the training phase to the present dataset for a better model.",
                                   "The predict type can be changed to align with the requirements for certain measures and more.",
                                   "If further information are needed, visist the GitHub repository in which all packages and algorithms are referenced",
                                   sep = " "),
                             paste("Decision Trees are adjustable via:",
                                   "xval - number of cross validations while computing the tree (set as static parameter)",
                                   "minsplit - minimum number of observations in a node before performing a split",
                                   "cp - complexity parameter to prevent a split if overvall lack of fit is not decreased by the factor of cp",
                                   "maxdepth - maximum depth of any node of the tree", sep = "<br/>"),
                             paste("Random Forests are adjustable via:",
                                   "num.trees - number of trees used for prediction",
                                   "mtry - number of features that are considered for splitting a node",
                                   "min.node.size - minimal node size to stop this node from splitting",
                                   sep = "<br/>"),
                             paste("Support Vector machines are adjustable via:",
                                   "kernel - a kernel function to quantify the similarity of two observations",
                                   #"linear kernel - expects a linear relationship between a pair of observations and quantifies the similarity using standard correlation",
                                   #"polynomial kernel - fitting the sv classifier into a higher dimensinoal feature space",
                                   #"radial kernel - kernel with very local behaviour",
                                   "cost - cost of constraint violations (allowed missclassifications)",
                                   "gamma - defines how far the influence of an observation reaches (low: far; high: close)",
                                   "degree - changes the decision boundary of the model to be more flexible towards the data",sep = "<br/>")),
                 Evaluate = c(paste("During the basic machine-learning workflow an algorithm is trained on a subset of the imported data, the training data,",
                                    "and evaluated on the remaining part of it, the test dataset.",
                                    "The target variable is predicted both for the training and test dataset in order to be able to compare different",
                                    "hyperparameter settings and take care of overfitting (loss of generalizability to new data).",
                                    "After predicting the target, a measure can be selected to score the performance of the trained model on both datasets.",
                                    "Alternatively, it is also possible to evaluate a learner using one of several resampling strategies such as holdout, cross-validation or bootstrap.",sep = " "),
                              paste("After creating learners in the 'learner'-tab, one can be selected for model training.",
                                    "The trained model serves to make a prediction for the response of the target.",
                                    "Different measures are available for performance evaluation afterwards.",
                                    "The classification error (ce) and accuracy (acc) can be selected as measures to score classification problems.",
                                    "In return, mean absolute error (mae) and mean standard error (mse) are available for regression problems.")),
                 Resample = c(paste("Resampling offers the possibility of training the same learner several times on different subsets of the",
                                    "training data.",
                                    "In doing so multipe models are generated, each differing slightly from the others.",
                                    "By evaluating the aggregated performance over all models, a better inference on the suitability",
                                    "of the algorithm and its hyperparameterset for the present machine learning problem",
                                    "can be made."),
                              paste("To start resampling choose a learner and a resampling strategy.",
                                    "The strategy can then be adjusted through its hyperparameters in the 'parameter settings'.",
                                    "Stratification can be used to make sure the ratio between the different values of the target is kept within each sample.",
                                    "After resampling, a combination of all generated models can be used to compute an overall prediction.",
                                    "The ResampleResult-object that mlr3 produces can be downloaded for further inspection.",
                                    "Finally, a measure can be used to compute the aggregated performance of the algorithm on given observations of the test sets.",
                                    sep = " "),
                              paste("A holdout strategy is the same as the training test split performed during the 'basic workflow'."),
                              paste("Cross validation splits the dataset in k groups of equal size.",
                                    "Each group serves as test set during one iteration while the others are used as training data.",
                                    "The number of iterations is therefore also the amount of groups (k) that are being generated.",
                                    sep = " "),
                              paste("The bootstrap strategy repeatedly (k times) generates random distinct samples of observations from the imported dataset.",
                                    "The sampling is done with replacement. The remaining data, out-of-bag samples, serve as test set for the respective",
                                    "iteration and the ratio is adjusted via changing the value of the data split.")),
                 Benchmark = c(paste("A benchmark experiment can be conducted to compare different learners on a task using the same resampling strategy.",
                                     "After generating a benchmark design, a cartesian product between all components, models for each learner",
                                     "are computed. An mlr3 BenchmarkResult-object is generated, storing every model  of each learner for each iteration.",
                                     "By analysing the results, the best algorithm and set of hyperparameters for the present dataset can be detected.",
                                     "Note that using many learners and iterations on a huge dataset increases computation time.",
                                     sep = " "),
                               paste("The workflow can be started on the left-hand side after selecting the learners to be included in the experiment as well as a",
                                     "resampling strategy.",
                                     "Hyperparameters for the resampling strategy can be adjusted similary to the 'Resampling' tab.",
                                     "An aggregated perfomance for every learner can be computed afterwards. The best learner will be displayed in the overview",
                                     "while the score of each learner is depicted in a table on the right-hand side.",
                                     "Also. the BenchmarkResult-object can be downloaded in order reuse it at a later time.",
                                     sep = " ")),
                 Predict = c(paste("After evaluating different learners and parameter sets, a final model can be computed on the entire training data",
                                   "using the most promising algorithm and settings.",
                                   "The learner can then be applied to a new dataset of the same structure as the training data to predict the response",
                                   "of the target variable for each observation. If the new dataset consists of more features than the training data only the feature subset",
                                   "used in training will be referenced for prediction.",
                                   sep = " "),
                             paste("The desired learner needs to be selected and trained.",
                                   "The resulting object can be downloaded for future use",
                                   "A new dataset can be imported with the same procedure as before.",
                                   "Afterwards, the trained model can be used to make a prediction which is displayed on the left-hand side.",
                                   "The prediction can be downloaded either as a csv-file or as a rds-file if it is to be included directly in another R session."))
                 )


#future TO-DO: implement additional learners, ultimately replace vector with as.data.table(mlr_learners)
possiblelearners <- c("Logistic Regression" = "classif.log_reg",
                      "Random Forest" = "classif.ranger", "Random Forest" = "regr.ranger",
                      "Decision Tree" = "classif.rpart",  "Decision Tree" = "regr.rpart",
                      "Support Vector Machine" = "classif.svm", "Support Vector Machine" = "regr.svm",
                      "Linear Regression" = "regr.lm")

#log_reg and lm without params
learnerparams <- list(ranger = c("num.trees", "mtry", "min.node.size"),
                     rpart = c("minsplit", "cp", "maxdepth"),
                     supportvm = c("kernel", "cost", "gamma", "degree")
                      )

possiblemeasures <- list(classif = c("classif.acc", "classif.ce"),
                         regr = c("regr.mae", "regr.mse"))


# condition handling
errorModal <- function(title, description, id, err) {
  showModal(
    modalDialog(
      title = h2(title, style = "text-align: center;"),
      h4(description),
      HTML("</br>"),
      h4(paste("Error:", err, sep =  " ")),
      easyClose = TRUE, fade = FALSE,
      footer = div(style = "display:inline-block;width:100%;text-align: center;",
                   actionButton(inputId = id, label = "OK", `data-dismiss` = "modal",
                                style = "background-color: rgb(174, 222, 244); padding: 10px 32px; margin: 26px 5px 0;")))
  )
}

errorAlertTrain <- function(error) {
  errorModal(title = "Model Training Failed",
             description = paste("It seems that the selected learner does not fully support the current data.",
                                 "Please review the dataset and check if the learner can work with all features present.",
                                 sep = " "),
             err = error$message,
             id = "okTrain")
}


errorAlertPredict <- function(error) {
  errorModal(title = "Predicting Target Failed",
             description = paste("Sorry, predicting the target variable of the training or test data did not work.",
                                 "Please have a look at the error message to get insight into the cause.", sep = " "),
             err = error$message,
             id = "okPredict")
}

errorAlertPredictNew <- function(error) {
  errorModal(title = "Predicting Target Failed",
             description = paste("Sorry, predicting the target variable of the newly imported dataset did not work.",
                                 "Please have a look at the error message to get insight into the cause.", sep = " "),
             err = error$message,
             id = "okPredict")
}

errorAlertResample <- function(error) {
  errorModal(title = "Resampling Failed",
             description = paste("Sorry, performing the resampling strategy did not work.",
                                 "Perhaps a resampling parameter did not support the value it was given,",
                                 "the selected learner is incompatible with some of the training data or",
                                 "the test data contain new factor levels that the model cannot handle.",
                                 "Please have a look at the error message to get insight into the cause.", sep = " "),
             err = error$message,
             id = "okResample")
}

errorAlertBench <- function(error) {
  errorModal(title = "Benchmark Failed",
             description = paste("Sorry, computing the benchmark failed.",
                                 "Perhaps a value of the resampling parameter settings is not supported,",
                                 "one of the selected learners is incompatible with some of the training data or",
                                 "the test data contain new factor levels that the model cannot handle.",
                                 "Otherwise, please have a look at the error message to get insight into the cause.", sep = " "),
             err = error$message,
             id = "okBench")
}


warningAlert <- function(warning) {
  showNotification(warning$message, type = "warning")
}

messageDetail <- function(msg) {
  showNotification(msg, type = "message")
}
