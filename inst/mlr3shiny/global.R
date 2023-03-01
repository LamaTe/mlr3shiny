library(shiny)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(DALEX)
library(DALEXtra)
library(Metrics)
library(plyr)
library(dplyr)
library(DT)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(data.table)
library(readxl)
library(stringr)
library(purrr)
library(stringr)
library(bslib)
library(haven)

requireNamespace("mlr3measures")

userhelp <- list(Data = c(paste("Hallo, 21.01.2023! This app let's you conduct the basic steps of a machine learning workflow using your own data.",
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
                                "Moreover, the meta information holds the task type such as a classification or a regression.",
                                "If the target variable is of type numeric, the task will be of type regression. If the target is a factor,",
                                "the task is a classification and can predict either a target with two levels (two class) or a target with multiple classes (multiclass).", sep = " "),
                          paste("Three examplary tasks are provided. Otherwise, select the imported training data and set a target name and a task ID.",
                                "On the right-hand side you can select features in case only a subset of variables available in the dataset should be included in the ML experiment.",
                                "This is useful to exclude variables that do not provide an information gain such as an ID or a feature with only one level over all observations.",
                                "To inspect the data, return to the 'Data' tab.",
                                "If it is a two class classification some evaluation metrics such as precision or recall look at 'positive' and 'negative' classes of the target variable.",
                                "If this needs changing, you can set the class that shall be seen as the positive one here.",
                                sep = " ")),

                 Learner = c(paste("Different machine-learning algorithms can be selected as 'learners' which",
                                   "provide an interface to the actual algorithm together wirth some meta information such as the predict type,",
                                   "and its current hyperparameter settings. For classification tasks predict types can be response or probability.",
                                   sep = " "),
                             paste("Currently, one can select the among algorithms: decision tree, random forest, xgboost, support vector machine.",
                                   "In addition logistic regression is available for classification and linear regression for regression.",
                                   "For each learner the most important hyperparameters can be specified under 'Learner Parameters' ",
                                   "in order to improve predictive power.",
                                   "The predict type can be changed to align with the requirements w.r.t. particular measures or desired usage of the model.",
                                   "For further information, visit the GitHub repository where all packages and algorithms are referenced.",
                                   sep = " "),
                             paste("Decision trees are adjustable via:",
                                   "minsplit - minimum number of observations in a node before performing a split,",
                                   "cp - larger complexity parameters > 0 prevents a split for small improvements,",
                                   "maxdepth - maximum depth of any node of the tree,", sep = "<br/>"),
                             paste("Random forests are adjustable via:",
                                   "num.trees - number of trees used for prediction,",
                                   "mtry - number of features that are considered for splitting a node,",
                                   "min.node.size - minimal node size to stop this node from splitting,",
                                   sep = "<br/>"),
                             paste("Support vector machines are adjustable via:",
                                   "kernel - a kernel function to quantify the similarity of two observations,",
                                   #"linear kernel - expects a linear relationship between a pair of observations and quantifies the similarity using standard correlation",
                                   #"polynomial kernel - fitting the sv classifier into a higher dimensional feature space",
                                   #"radial kernel - kernel with very local behaviour",
                                   "cost - cost of constraint violations (allowed missclassifications),",
                                   "gamma - defines how far the influence of an observation reaches (low: far; high: close),",
                                   "degree - a higher polynomial degree increases adaptivity to data,", sep = "<br/>"),
                             paste("xgboost is adjustable via:",
                                   "eta - controls the learning rate: scales the contribution of each tree by a factor < 1.
                                    A lower value for eta is more robust to overfitting but requires larger value of nrounds,",
                                   "max_depth - maximum depth of a tree,",
                                   "nrounds - maximum number of boosting iterations,",
                                   "colsample_bytree - subsample ratio of columns when constructing each tree,",
                                   "booster - which booster to use.",
                                   sep = "<br/>")),
                 Evaluate = c(paste("During the basic machine-learning workflow an algorithm is trained on a subset of the imported data, the training data,",
                                    "and evaluated on the remaining part of it, the test dataset.",
                                    "The target variable is predicted both for the training and test dataset in order to be able to compare different",
                                    "hyperparameter settings and take care of overfitting (loss of generalizability to new data).",
                                    "After predicting the target, a measure can be selected to score the performance of the trained model on both datasets.",
                                    "Alternatively, it is also possible to evaluate a learner using one of several resampling strategies such as holdout, cross-validation or bootstrap.",sep = " "),
                              paste("After creating learners in the 'learner'-tab, one learner can be selected for model training.",
                                    "The trained model serves to make a prediction for the target variable.",
                                    "Different measures are available for performance evaluation afterwards depending on the characteristics of the task and the learner.",
                                    "Classification metrics differ between two class vs multi class prediction problems. Certain measures require probabilities to be computed.",
                                    "This can be specified under Learner - Change Predict Type.",
                                    "<p>Further information on each measure can be found in the <a href='https://mlr3measures.mlr-org.com/reference/index.html'>mlr3 function reference</a>.</p>"
                                )),
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
                                   "The prediction can be downloaded either as a csv-file or as a rds-file if it is to be included directly in another R session.")),
                 Explain = c(paste("For further use in different domains, it can be of considerable advantage to understand the internal \"ways of thinking\" of the algorithms used, which lead to the eventual calculation of the results. Following the Explainable AI approach, model agnostic methods are used here via the <a href='https://github.com/christophM/iml'>iml</a> framework to graphically represent the Feature Importance and Partial Dependence (PD)-Plots.
As a result, even more complex models can be made relatively easy to understand, which can be particularly useful in arguing for the use of machine learning in newer application areas. <p> More information regarding PD-Plots and Variable Importance can be found in <a href=\"https://christophm.github.io/interpretable-ml-book/index.html\">Interpretable Machine Learning</a> which is in close relation to iml itself.</p>",
                                   sep = " "),
                             paste("After selecting a learner that has been trained on all data in advance, the loss function can be selected that is to be used in the calculation of the feature importance. Additionally, the compare method can be used to select whether the importance should be displayed as a difference (error.permutation-error.original) or as a ratio (error.permutation/error.original). Finally, the features to be displayed in the PD plot can also be selected.
                             <p>Further information on the used loss functions can be found in the <a href=\"https://github.com/mfrasco/Metrics\">Metrics Github-Repository</a></p>")

                 )
)

#future TO-DO: implement additional learners, ultimately replace vector with as.data.table(mlr_learners)
possiblelearners <- c("Logistic Regression" = "log_reg",
                      "Random Forest" = "ranger",
                      "Decision Tree" = "rpart",
                      "Support Vector Machine" = "svm",
                      "Linear Regression" = "lm",
                      "Extreme Gradient Boosting (xgboost)" = "xgboost")

#log_reg and lm without params
learnerparams <- list(ranger = c("num.trees", "mtry", "min.node.size"),
                     rpart = c("minsplit", "cp", "maxdepth"),
                     supportvm = c("kernel", "cost", "gamma", "degree"),
                     xgboost = c("eta", "max_depth", "nrounds", "colsample_bytree", "booster")
                      )


msr_translations <- data.table(key_msr = c('classif.acc', 'classif.bacc', 'classif.ce', 'classif.logloss',
                                           'classif.auc', 'classif.fnr', 'classif.fpr', 'classif.npv',
                                           'classif.precision', 'classif.specificity', 'classif.recall',
                                           'regr.mae', 'regr.mse', 'regr.rmse', 'regr.rsq'),
                               name = c('accuracy', 'balanced accuracy', 'classification error', 'log loss',
                                        'area under the ROC curve', 'false negative rate', 'false positive rate', 'negative predictive value',
                                        'positive predictive value (precision)', 'true negative rate (specificity)', 'true positive rate (recall/sensitivity)',
                                        'mean absolute error', 'mean squared error', 'root mean squared error', 'r squared'
))

avail_msrs <- as.data.table(mlr_measures) # get all mlr3 measures
get_msrs <- function(current_task, current_learner, available_measures, measure_translations){
  # Checks for task type and subtype and returns available measures for input selection option
  # parameters: current task, current_learner, all measures, measure translations
  # output: mlr3 measure and real name

  make_named_vec <- function(options) {
    temp_opts <- as.list(options[[1]])
    names(temp_opts) <- options[[2]]
    return(temp_opts)
  }
  if (current_task$task_type == 'regr'){
    keys <- available_measures[which(available_measures$task_type == current_task$task_type)][['key']]
  }
  # check for classif as regression has no subtypes (properties)
  else if (current_task$task_type == 'classif' & current_task$properties == 'twoclass') {
    keys <- available_measures[which(available_measures$task_type == 'classif' &
                                       available_measures$predict_type == 'response' |
                                       available_measures$predict_type == 'prob')][['key']]
      }
  else  {
    keys <- intersect(available_measures[which(available_measures$task_type == current_task$task_type &
                                                 available_measures$predict_type == current_learner$predict_type)][['key']],
                      available_measures[which(sapply(available_measures$task_properties, function(x) !is.element('twoclass', x)))][['key']])
  }
  options <- measure_translations[which(measure_translations$key_msr %in% keys), ]
  return(make_named_vec(options))
}

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

errorAlertBenchAggr <- function(error) {
  errorModal(title = "Benchmark Aggregation Failed",
             description = paste("Sorry, calculating an aggregated performance for the benchmark failed.",
                                 "Most likely, the selected learners have different prediction types.",
                                 "For the calculation to succeed, all learners need the same prediction type - response or probability.",
                                 "You can set the prediction type in the Learner-Tab.",
                                 sep = " "),
             err = error$message,
             id = "okBench")
}


warningAlert <- function(warning) {
  showNotification(warning$message, type = "warning")
}

messageDetail <- function(msg) {
  showNotification(msg, type = "message")
}
