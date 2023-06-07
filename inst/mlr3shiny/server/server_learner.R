# each Learner has its own reactive values
LearnerMeta <- reactiveValues(Count = 1, learner_choice = NULL, Learner_Avail = NULL, TwoClass = NULL)
Learner1 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL, Learner_Name = NULL)
Learner2 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL, Learner_Name = NULL)
Learner3 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL, Learner_Name = NULL)
Learner4 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL, Learner_Name = NULL)
Learner5 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL, Learner_Name = NULL)
Learner6 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL, Learner_Name = NULL)
Learner7 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL, Learner_Name = NULL)

# list of trained learners (will be dynamically filled in the training process)
trained_learner_list <- reactiveValues()

# reset a single trained learner
# used when the learner type or params are changed
reset_single_trained_learner <- function(learner_name) {
   if (!is.null(trained_learner_list)) {
      trained_learner_list[[learner_name]] <- NULL
   }
}

# reset the whole list of trained learners
reset_trained_learner_list <- function() {
   # iterating over all trained learners and resetting them
   # See: https://stackoverflow.com/questions/61887112/how-to-reset-reactivevalues
   for (trained_learner in names(trained_learner_list)) {
      trained_learner_list[[trained_learner]] <- NULL
   }
}

# check whether it is a regression or classification and then give possible learners for each
observe({
   if (!is.null(currenttask$task) && currenttask$task$task_type == "classif") {
      basic_choice <- c("decision tree" = "classif.rpart", "random forest" = "classif.ranger", "support vector machine" = "classif.svm", "xgboost" = "classif.xgboost")
      if (currenttask$task$properties == "multiclass") {
         LearnerMeta$learner_choice <- basic_choice
      } else {
         LearnerMeta$TwoClass <- TRUE
         LearnerMeta$learner_choice <- c(basic_choice, "logistic regression" = "classif.log_reg")
      }
   } else if (!is.null(currenttask$task) && currenttask$task$task_type == "regr") {
      LearnerMeta$learner_choice <- c(
         "decision tree" = "regr.rpart", "linear regression" = "regr.lm", "random forest" = "regr.ranger",
         "support vector machine" = "regr.svm", "xgboost" = "regr.xgboost"
      )
   }
})

# let's user add another learner object
addLearner <- function(LearnNumber) {
   tagList(
      fluidRow(
         column(
            3,
            h5(paste("Learner", (LearnNumber), sep = " "))
         ),
         column(
            6,
            selectInput(inputId = paste0("Learner_Learner", LearnNumber), label = NULL, choices = LearnerMeta$learner_choice, selected = "decision tree")
         ),
         column(
            3,
            actionButton(
               inputId = paste0("Learner_Create", LearnNumber), label = "Go", icon = icon("filter"),
               style = "float:right;"
            )
         )
      )
   )
}

output$Learner_other_Learner1 <- renderUI({
   addLearner(LearnNumber = 1)
})

# ui for each learner selection with name, choices and go button
observeEvent(input$Learner_add, {
   LearnerMeta$Count <- LearnerMeta$Count + 1
   currentcount <- LearnerMeta$Count
   switch(as.character(currentcount),
      "2" = {
         output$Learner_other_Learner2 <- renderUI({
            addLearner(currentcount)
         })
      },
      "3" = {
         output$Learner_other_Learner3 <- renderUI({
            addLearner(currentcount)
         })
      },
      "4" = {
         output$Learner_other_Learner4 <- renderUI({
            addLearner(currentcount)
         })
      },
      "5" = {
         output$Learner_other_Learner5 <- renderUI({
            addLearner(currentcount)
         })
      },
      "6" = {
         output$Learner_other_Learner6 <- renderUI({
            addLearner(currentcount)
         })
      },
      "7" = {
         output$Learner_other_Learner7 <- renderUI({
            addLearner(currentcount)
         })
      },
      {
         shinyalert(
            title = "Maximum Reached",
            text = "Currently a maximum of 7 learners at the same time is supported.",
            animation = FALSE, closeOnClickOutside = TRUE
         )
      }
   )
})

# only show learner panels when at least one is created otherwise give user info that no learner has been created yet
observe({
   checkcondition <- is.null(c(
      Learner1$Learner, Learner2$Learner, Learner3$Learner, Learner4$Learner,
      Learner5$Learner, Learner6$Learner, Learner7$Learner
   ))
   toggle(id = "Learner_NA", condition = checkcondition)
   toggle(id = "Learner_Learners", condition = !checkcondition)
})

hideorshowTab <- function(learnerobject, target) {
   if (is.null(learnerobject)) {
      hideTab(inputId = "Learner_Learners_Tab", target = target)
   } else {
      showTab(inputId = "Learner_Learners_Tab", target = target, select = TRUE)
   }
}

# for now this way, in case removal of learners is implemented.
# else, place function in observeEvent for each Learner
observe({
   hideorshowTab(Learner1$Learner, "Learner 1")
   hideorshowTab(Learner2$Learner, "Learner 2")
   hideorshowTab(Learner3$Learner, "Learner 3")
   hideorshowTab(Learner4$Learner, "Learner 4")
   hideorshowTab(Learner5$Learner, "Learner 5")
   hideorshowTab(Learner6$Learner, "Learner 6")
   hideorshowTab(Learner7$Learner, "Learner 7")
})

# Learner tabs
## Learner overview

# get set params
getCurrentParams <- function(learnerobject) {
   selectedParams <- character(0)
   for (i in names(learnerobject$Learner$param_set$values)) {
      selectedParams <- paste(c(selectedParams, paste(i, learnerobject$Learner$param_set$values[[i]],
         sep = ": "
      )), collapse = ", ")
   }
   return(selectedParams)
}

getLearnerOverview <- function(learnerobject) {
   overview <- list(
      "name" = getDisplayLearnerName(learnerobject),
      "predict type" = learnerobject$Learner$predict_type,
      # do not include params as svm still remembers parameter set for radial kernel such as gamma, even though it is not
      # used in linear kernel -> confusing for user
      # "params" = getCurrentParams(learnerobject = learnerobject),
      # using the original learner object because "predict_types"
      # cant be retrieved from a learner returned from as_learner()
      "predict types" = learnerobject$Learner$predict_types,
      "properties" = learnerobject$Learner$properties,
      "feature types" = learnerobject$Learner$feature_types
   )
   return(overview)
}

getDisplayLearnerName <- function(learnerobject) {
   for (learnername in possiblelearners) {
      if (grepl(learnername, learnerobject$Learner$id, fixed = TRUE)) {
         return(names(which(possiblelearners == learnername)))
      }
   }
}

addOverviewLineLearner <- function(title, body) {
   fluidRow(
      column(6, h5(title)),
      column(6, h5(body))
   )
}

makeOverviewUi <- function(learnerobject) {
   overviewUi <- tagList(
      fluidRow(
         column(
            6,
            addOverviewLineLearner("Algorithm: ", learnerobject$Overview[[1]]),
            addOverviewLineLearner("Current Predict Type: ", learnerobject$Overview[[2]]),
            # addOverviewLineLearner("Current Parameter: ", paste(learnerobject$Overview[[3]], collapse = ", "))
            addOverviewLineLearner("Supported Predict Types: ", paste(learnerobject$Overview[[3]], collapse = ", "))
         ),
         column(
            6,
            # addOverviewLineLearner("Supported Predict Types: ", paste(learnerobject$Overview[[4]], collapse = ", ")),
            addOverviewLineLearner("Properties: ", paste(learnerobject$Overview[[4]], collapse = ", ")),
            addOverviewLineLearner("Supported Feature Types: ", paste(learnerobject$Overview[[5]], collapse = ", "))
         )
      )
   )
   return(overviewUi)
}

## Learner params
addNumericParam <- function(id, lower, upper, learnername, default, stpsize = 1) {
   if (missing(default)) {
      default <- 0
   }
   fluidRow(
      column(
         3,
         h5(id)
      ),
      column(
         3,
         h5(paste("Lower:", lower, sep = " "))
      ),
      column(
         3,
         h5(paste("Upper:", upper, sep = " "))
      ),
      column(
         3,
         numericInput(
            inputId = paste0(learnername, "Param", id), label = NULL,
            value = default, min = lower, max = upper, step = stpsize
         )
      )
   )
}



addFactorParam <- function(id, levels, learnername, default) {
   fluidRow(
      column(
         3,
         h5(id)
      ),
      column(
         6,
         h5(paste("Levels:", paste(levels, collapse = ", "), sep = " "))
      ),
      column(
         3,
         selectInput(inputId = paste0(learnername, "factor"), label = NULL, choices = levels, selected = default)
      )
   )
}

# for svm to restrict parameter choice for now
# however, now this is not a good expandable solution - just a quick one; better to just offer all params and drop unused ones silently

getKernelParams <- function(learnerobject, learnername, selectedkernel) {
   # Input: current mlr3 learner, learner name (learner1, learner2 etc), user input for kernel
   # Generates a ui taglist to display additional hyperparameter that are available for selected kernel
   # Implicitly updates available hyperparameters depending on kernel for later hyperparameter adjustment (not ideal - function with side effects!)
   # Returns: taglist for kernelparamet

   if (selectedkernel == "polynomial") {
      kernelparams <- tagList(
         addNumericParam(
            id = paste0(learnerobject$Learner_Name, ".", "gamma"), lower = learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "gamma")]]$lower,
            upper = learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "gamma")]]$upper,
            learnername = learnername, default = learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "gamma")]]$default,
            stpsize = 0.1
         ),
         addNumericParam(
            id = paste0(learnerobject$Learner_Name, ".", "degree"), lower = learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "degree")]]$lower,
            upper = learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "degree")]]$upper,
            learnername = learnername, default = learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "degree")]]$default
         )
      )
      # update learnerobject$Params so that only the hyperparams are set that are actually available
      learnerobject$Params <- c(
         learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "kernel")]],
         learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "cost")]],
         learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "gamma")]],
         learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "degree")]]
      )
      return(kernelparams)
   } else if (selectedkernel == "radial" || selectedkernel == "sigmoid") {
      kernelparams <- tagList(
         addNumericParam(
            id = paste0(learnerobject$Learner_Name, ".", "gamma"), lower = learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "gamma")]]$lower, upper = learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "gamma")]]$upper,
            learnername = learnername, default = learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "gamma")]]$default,
            stpsize = 0.1
         )
      )
      learnerobject$Params <- c(
         learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "kernel")]],
         learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "cost")]],
         learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "gamma")]]
      )
      return(kernelparams)
   } else {
      learnerobject$Params <- c(
         learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "kernel")]],
         learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", "cost")]]
      )
      return(NULL)
   }
}

getAvailableParams <- function(algorithm, learnerobject) {
   # Input: Selected algorithm as defined in global, learnerobject that refers to an mlr3 learner
   # Selects available parameters for given algorithm from global and gets parameter details through mlr3
   # Implicitly assigns available hyperparameter to learner$Params for later reference when setting hyperparams
   # Output: list of parameters with id, lower and upper levels, defaults
   params <- list()
   if (grepl("log_reg", learnerobject$Learner$id)) {
     params[[length(params) + 1]] <- learnerobject$Learner$param_set$params[["threshold.thresholds"]]
   }
   else{
     for (i in 1:length(learnerparams[[algorithm]])) {
        # concatenating the learner id (e.g. "classif.rpart") with a . and the actual parameter
        # this is required because the graph learner stores all parameters in this format
        # example: "classif.rpart.maxdepth"
        params[[i]] <- learnerobject$Learner$param_set$params[[paste0(learnerobject$Learner_Name, ".", learnerparams[[algorithm]][i])]]
     }
     if (grepl("threshold", learnerobject$Learner$id)) {
        params[[length(params) + 1]] <- learnerobject$Learner$param_set$params[["threshold.thresholds"]]
     }
   }
   learnerobject$Params <- params
   return(params)
}

# define the parameter settings UI for each learner depending on the selected algorithm
makeParamUi <- function(learnerobject, learnername) {
   # Input: mlr3 learner object, name of currently selected learner (learner1, learner2 etc)
   # Creates layout for hyperparameters as a taglist depending on the algorithm
   # Returns: taglist of inputs for Learner hyperparameters
   parameterui <- NULL

   if (learnerobject$Learner$param_set$is_empty) {
      return(h5("No Parameters available to be set.", style = "text-align: center;"))
   } else if (grepl("ranger", learnerobject$Learner$id, fixed = TRUE)) {
      params <- getAvailableParams(algorithm = "ranger", learnerobject = learnerobject)
      # TO-DO: Get a better solution - ugly and repetitive
      if (grepl("threshold", learnerobject$Learner$id)) {
         parameterui <- tagList(
            # num.trees
            addNumericParam(
               id = params[[1]]$id, lower = params[[1]]$lower, upper = params[[1]]$upper, learnername = learnername,
               default = params[[1]]$default
            ),
            # mtry upper needs to be restricted to number of features since mlr ships with Inf as upper value
            addNumericParam(
               id = params[[2]]$id, lower = params[[2]]$lower, upper = length(currenttask$task$feature_names), learnername = learnername,
               default = params[[2]]$default
            ),
            # min.node.size
            addNumericParam(
               id = params[[3]]$id, lower = params[[3]]$lower, upper = params[[3]]$upper, learnername = learnername,
               default = params[[3]]$default
            ),
            addNumericParam(id = params[[length(params)]]$id, lower = 0, upper = 1, learnername = learnername, default = 0.5, stpsize = 0.1),
            actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
         )
      } else {
         parameterui <- tagList(
            # num.trees
            addNumericParam(
               id = params[[1]]$id, lower = params[[1]]$lower, upper = params[[1]]$upper, learnername = learnername,
               default = params[[1]]$default
            ),
            # mtry upper needs to be restricted to number of features since mlr ships with Inf as upper value
            addNumericParam(
               id = params[[2]]$id, lower = params[[2]]$lower, upper = length(currenttask$task$feature_names), learnername = learnername,
               default = params[[2]]$default
            ),
            # min.node.size
            addNumericParam(
               id = params[[3]]$id, lower = params[[3]]$lower, upper = params[[3]]$upper, learnername = learnername,
               default = params[[3]]$default
            ),
            actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
         )
      }
   } else if (grepl("rpart", learnerobject$Learner$id, fixed = TRUE)) {
      params <- getAvailableParams(algorithm = "rpart", learnerobject = learnerobject)
      if (grepl("threshold", learnerobject$Learner$id)) {
         parameterui <- tagList(
            addNumericParam(
               id = params[[1]]$id, lower = params[[1]]$lower, upper = params[[1]]$upper, learnername = learnername,
               default = params[[1]]$default, stpsize = 1
            ),
            addNumericParam(
               id = params[[2]]$id, lower = params[[2]]$lower, upper = params[[2]]$upper, learnername = learnername,
               default = params[[2]]$default, stpsize = 0.0025
            ),
            addNumericParam(
               id = params[[3]]$id, lower = params[[3]]$lower, upper = params[[3]]$upper, learnername = learnername,
               default = params[[3]]$default, stpsize = 1
            ),
            addNumericParam(id = params[[length(params)]]$id, lower = 0, upper = 1, learnername = learnername, default = 0.5, stpsize = 0.1),
            actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
         )
      } else {
         parameterui <- tagList(
            addNumericParam(
               id = params[[1]]$id, lower = params[[1]]$lower, upper = params[[1]]$upper, learnername = learnername,
               default = params[[1]]$default, stpsize = 1
            ),
            addNumericParam(
               id = params[[2]]$id, lower = params[[2]]$lower, upper = params[[2]]$upper, learnername = learnername,
               default = params[[2]]$default, stpsize = 0.0025
            ),
            addNumericParam(
               id = params[[3]]$id, lower = params[[3]]$lower, upper = params[[3]]$upper, learnername = learnername,
               default = params[[3]]$default, stpsize = 1
            ),
            actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
         )
      }
   } else if (grepl("svm", learnerobject$Learner$id, fixed = TRUE)) {
      params <- getAvailableParams(algorithm = "supportvm", learnerobject = learnerobject)
      if (grepl("threshold", learnerobject$Learner$id)) {
         parameterui <- tagList(
            # sigmoid kernel removed for explanatory reasons
            addFactorParam(id = params[[1]]$id, levels = c("radial", "polynomial", "linear"), learnername = learnername, default = params[[1]]$default),
            addNumericParam(
               id = params[[2]]$id, lower = params[[2]]$lower, upper = params[[2]]$upper, learnername = learnername,
               default = params[[2]]$default, stpsize = 0.1
            ),
            uiOutput(outputId = paste0(learnername, "KernelParam", "kernel")), # depending on selected kernel, different hyperparameters are available
            addNumericParam(id = params[[length(params)]]$id, lower = 0, upper = 1, learnername = learnername, default = 0.5, stpsize = 0.1),
            actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
         )
      } else {
         parameterui <- tagList(
            # sigmoid kernel removed for explanatory reasons
            addFactorParam(id = params[[1]]$id, levels = c("radial", "polynomial", "linear"), learnername = learnername, default = params[[1]]$default),
            addNumericParam(
               id = params[[2]]$id, lower = params[[2]]$lower, upper = params[[2]]$upper, learnername = learnername,
               default = params[[2]]$default, stpsize = 0.1
            ),
            uiOutput(outputId = paste0(learnername, "KernelParam", "kernel")), # depending on selected kernel, different hyperparameters are available
            actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
         )
      }
   } else if (grepl("xgboost", learnerobject$Learner$id, fixed = TRUE)) {
      params <- getAvailableParams(algorithm = "xgboost", learnerobject = learnerobject)
      if (grepl("threshold", learnerobject$Learner$id)) {
         parameterui <- tagList(
            addNumericParam(id = params[[1]]$id, lower = params[[1]]$lower, upper = params[[1]]$upper, learnername = learnername, default = params[[1]]$default, stpsize = 0.1),
            addNumericParam(id = params[[2]]$id, lower = params[[2]]$lower, upper = params[[2]]$upper, learnername = learnername, default = params[[2]]$default, stpsize = 1),
            addNumericParam(id = params[[3]]$id, lower = params[[3]]$lower, upper = params[[3]]$upper, learnername = learnername, default = params[[3]]$default, stpsize = 1),
            addNumericParam(id = params[[4]]$id, lower = params[[4]]$lower, upper = params[[4]]$upper, learnername = learnername, default = params[[4]]$default, stpsize = 0.1),
            addFactorParam(id = params[[5]]$id, levels = c("gblinear", "gbtree", "dart"), learnername = learnername, default = params[[5]]$default),
            addNumericParam(id = params[[length(params)]]$id, lower = 0, upper = 1, learnername = learnername, default = 0.5, stpsize = 0.1),
            actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
         )
      } else {
         parameterui <- tagList(
            addNumericParam(id = params[[1]]$id, lower = params[[1]]$lower, upper = params[[1]]$upper, learnername = learnername, default = params[[1]]$default, stpsize = 0.1),
            addNumericParam(id = params[[2]]$id, lower = params[[2]]$lower, upper = params[[2]]$upper, learnername = learnername, default = params[[2]]$default, stpsize = 1),
            addNumericParam(id = params[[3]]$id, lower = params[[3]]$lower, upper = params[[3]]$upper, learnername = learnername, default = params[[3]]$default, stpsize = 1),
            addNumericParam(id = params[[4]]$id, lower = params[[4]]$lower, upper = params[[4]]$upper, learnername = learnername, default = params[[4]]$default, stpsize = 0.1),
            addFactorParam(id = params[[5]]$id, levels = c("gblinear", "gbtree", "dart"), learnername = learnername, default = params[[5]]$default),
            actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
         )
      }
   }
     else if (grepl("log_reg", learnerobject$Learner$id, fixed = TRUE)) {
        params <- getAvailableParams(algorithm = "log_reg", learnerobject = learnerobject)
        if (grepl("threshold", learnerobject$Learner$id)) {
          parameterui <- tagList(
            addNumericParam(id = params[[length(params)]]$id, lower = 0, upper = 1, learnername = learnername, default = 0.5, stpsize = 0.1),
            actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
          )
        }
   }
   return(parameterui)
}

# make the overview for each learner
makeLearnerOvTab <- function(learnerobject) {
   learnerov <- tagList(
      wellPanel(
         fluidRow(
            column(
               12,
               h5("Learner Information", style = "font-weight: bold;"),
               makeOverviewUi(learnerobject = learnerobject)
            )
         )
      )
   )
}

# place overview and learner together in a tab
makeLearnerParamTab <- function(learnerobject, learnername) {
   # twoclass classif cannot change predict type due to pipeops
   if (!identical(currenttask$task$properties, character(0)) && currenttask$task$properties == "twoclass") {
      change_predict_type <- tagList()
   } else {
      change_predict_type <- tagList(
         hr(style = "border-color: #3e3f3a;"),
         fluidRow(
            column(
               4,
               h5("Change Predict Type")
            ),
            column(
               4,
               selectInput(
                  inputId = paste0(learnername, "PredictTypeChoice"), label = NULL, choices = learnerobject$Learner$predict_types,
                  selected = learnerobject$Learner$predict_type
               )
            ),
            column(
               4,
               actionButton(inputId = paste0(learnername, "PredictTypeChange"), label = "Change", style = "float: right;")
            )
         )
      )
   }
   learnerparams <- tagList(
      wellPanel(
         fluidRow(
            column(
               12,
               h5("Learner Parameters", style = "font-weight: bold;"),
               makeParamUi(learnerobject = learnerobject, learnername = learnername),
               ),
         ),
         change_predict_type
      )
   )
   return(learnerparams)
}


# # create GraphLearner object
# # for twoclass the threshold-po is added
# # if xgboost is selected the factor variables will be converted into numerals
# # furthermore ordered variables will also be converted to integers
# createGraphLearner_old_alex <- function(selectedlearner) {
#    graph <- Graph$new()
#    if (!isTRUE(currenttask$task$properties == "twoclass")) {
#       learner <- po("learner", lrn(input[[selectedlearner]]))
#       graph$add_pipeop(learner)
#    } else {
#       learner <- po("learner", lrn(input[[selectedlearner]], predict_type = "prob"))
#       graph$add_pipeop(learner)
#       graph <- graph %>>% po("threshold")
#    }
#    if (any(grepl("factor", unique(currenttask$task$feature_types$type))) && !any(grepl("factor", lrn(input[[selectedlearner]])$feature_types))) {
#       graph <- po("encode", method = "treatment", affect_columns = selector_type("factor")) %>>% graph
#    }
#    if (any(grepl("ordered", unique(currenttask$task$feature_types$type))) && !any(grepl("ordered", lrn(input[[selectedlearner]])$feature_types))) {
#       graph <- po("colapply", applicator = as.integer, affect_columns = selector_type("ordered")) %>>% graph
#    }
#    return(as_learner(graph))
# }


createGraphLearner <- function(selectedlearner) {
  if (!isTRUE(currenttask$task$properties == "twoclass")) {
    learner <- lrn(input[[selectedlearner]]) 
  } else { # ...otherwise predict_type = "prob" is set and a threshold po added below
    learner <- lrn(input[[selectedlearner]], predict_type = "prob")
  }
  if(input[["Task_robustify"]]){
    impm <- NULL 
    if(input[["impute_missings"]] == "TRUE") {impm <- TRUE}
    if(input[["impute_missings"]] == "FALSE") {impm <- FALSE}
    
    ftn <- NULL 
    if(input[["factors_to_numeric"]] == "TRUE") {ftn <- TRUE}
    if(input[["factors_to_numeric"]] == "FALSE") {ftn <- FALSE}
     
     graph <- pipeline_robustify(currenttask$task, learner,
                                 impute_missings    = impm,
                                 factors_to_numeric = ftn,
                                 max_cardinality    = input[["max_cardinality"]],
                                 ordered_action     = input[["ordered_action"]],
                                 character_action   = input[["character_action"]],
                                 POSIXct_action     = input[["POSIXct_action"]]) %>>% learner
  } else graph <- as_graph(po("learner", learner))
  #plot(graph)  
  if (isTRUE(currenttask$task$properties == "twoclass")) graph <- graph %>>% po("threshold")
  
  return(as_learner(graph))
}


# add observers and others to generate the tabs depending on the needs of the user
makeLearner <- function(learnerobject, learnername, trigger, selectedlearner, learnerparamoutput, learnerovoutput) {
   observeEvent(input[[trigger]], {
       learnerobject$Learner <- createGraphLearner(selectedlearner)
      learnerobject$Learner_Name <- input[[selectedlearner]]
      # learnerobject$Learner <- mlr_learners$get(input[[selectedlearner]])
      LearnerMeta$Learner_Avail <- unique(sort(c(LearnerMeta$Learner_Avail, learnername)))
      learnerobject$Hash <- learnerobject$Learner$hash
      output[[learnerparamoutput]] <- renderUI({
         makeLearnerParamTab(learnerobject = learnerobject, learnername = learnername)
      })
      output[[learnerovoutput]] <- renderUI({
         makeLearnerOvTab(learnerobject = learnerobject)
      })
      # resetting the trained learner on learner creation
      reset_single_trained_learner(learnername)
   })

   observe({
      if (!is.null(learnerobject$Learner)) {
         learnerobject$Overview <- getLearnerOverview(learnerobject = learnerobject)
      }
      learnerobject$Hash <- learnerobject$Learner$hash
   })

   # missing reactivity with R6-classes requires another execution of getLearnerOverview
   observeEvent(input[[paste0(learnername, "PredictTypeChange")]], {
      learnerobject$Learner$predict_type <- input[[paste0(learnername, "PredictTypeChoice")]]
      learnerobject$Overview <- getLearnerOverview(learnerobject = learnerobject)
      learnerobject$Hash <- learnerobject$Learner$hash
   })

   # kernel params for svm
   output[[paste0(learnername, "KernelParam", "kernel")]] <- renderUI({

      # if statement necessary to avoid strange shiny internal warning about empty condition when generating HTML
      if (input[[selectedlearner]] %in% c("classif.svm", "regr.svm")) {
         getKernelParams(
            learnerobject = learnerobject, learnername = learnername,
            selectedkernel = input[[paste0(learnername, "factor")]]
         )
      }
   })

   # # To-Do: get a prettier solution
   observeEvent(input[[paste0(learnername, "ChangeParams")]], {
      paramlist <- list()
      invalidparams <- NULL
      for (i in learnerobject$Params) {
         currentinput <- input[[paste0(learnername, "Param", i$id)]]
         # validate input value with 2 overall if statements 
         # on Windows and Linux Shiny sends NA (empty) inputs differently
         # Windows translates to 0 whereas Linux keeps as NA
         # hence we check if the input is NA separately
         if (is.null(currentinput) || is.na(currentinput)) {
           invalidparams <- c(invalidparams, i$id)
         }
         
         if (!is.null(currentinput) & !is.na(currentinput)) {
            if ((!is.na(learnerobject$Learner$param_set$params[[i$id]]$upper) &&
                 currentinput > learnerobject$Learner$param_set$params[[i$id]]$upper) ||
                (!is.na(learnerobject$Learner$param_set$params[[i$id]]$lower) &&
                 currentinput < learnerobject$Learner$param_set$params[[i$id]]$lower) ||
                (i$id == "classif.ranger.mtry" && currentinput > length(currenttask$task$feature_names)) ||
                (i$id == "threshold.thresholds" && currentinput > 1) ||
                (i$id == "threshold.thresholds" && currentinput < 0)) 
                {
                  invalidparams <- c(invalidparams, paste0(learnername, "Param", i$id))
            }
            else {
               paramlist[[i$id]] <- currentinput
            }
         }
      }

      if(!is.null(invalidparams)){
        shinyalert(title = "Empty or Invalid Parameter Input",
                text = paste("It seems that you tried to set parameter(s): ",
                             paste(unlist(invalidparams), collapse = ', '),
                             " that are left empty or not within their parameter range. The default value for the parameter(s) is used instead."),
                animation = FALSE, closeOnClickOutside = TRUE)
      }
        
      #TODO: need to find better solution to add factor params to paramlist
      # a solution could be to distinguish between a display name and the internal one
      # so the ChangeParams-Routine could be used


      svm_kernel <- c("radial", "polynomial", "linear")

      if (grepl("xgboost", learnerobject$Learner_Name)) {
         xgboost_booster <- c("gblinear", "gbtree", "dart")
         if (input[[paste0(learnername, "factor")]] %in% xgboost_booster) {
            paramlist[[paste0(learnerobject$Learner_Name, ".booster")]] <- input[[paste0(learnername, "factor")]]
         }
      }
      if (grepl("svm", learnerobject$Learner_Name)) {
         if (input[[paste0(learnername, "factor")]] %in% svm_kernel) {
            paramlist[[paste0(learnerobject$Learner_Name, ".kernel")]] <- input[[paste0(learnername, "factor")]]
         }
      }

      # explicit defaults for svm type to be used
      if (grepl("classif.svm", learnerobject$Learner_Name)) {
         paramlist[[paste0(learnerobject$Learner_Name, ".", "type")]] <- "C-classification"
      } else if (grepl("regr.svm", learnerobject$Learner_Name)) {
         paramlist[[paste0(learnerobject$Learner_Name, ".", "type")]] <- "eps-regression"
      }

     # check for any parameters that are to be overwritten by paramlist
     if(length(learnerobject$Learner$param_set$values) > 0){
       for (pn in names(learnerobject$Learner$param_set$values)){
         if (any(pn == names(paramlist))) {learnerobject$Learner$param_set$values[[pn]] <- NULL}
       }
     }

      #old: learnerobject$Learner$param_set$values <- paramlist # update hyperparameter values of current learner
      learnerobject$Learner$param_set$values <- c(learnerobject$Learner$param_set$values, paramlist) # update hyperparameter values of current learner
      # is new learnerobject$Learner$param_set$values still compatible with (unchanged) rest of learnerobject$Learner$param_set?
      # learnerobject$Overview <- getLearnerOverview(learnerobject = learnerobject)
      learnerobject$Hash <- learnerobject$Learner$hash

      # resetting trained learner when params change
      reset_single_trained_learner(learnername)
   })
}


# TO-DO: ugly code, think of better solution eg add simple for loop
makeLearner(
   learnerobject = Learner1, learnername = "Learner1", trigger = "Learner_Create1", selectedlearner = "Learner_Learner1",
   learnerparamoutput = "Learner1_tab", learnerovoutput = "Learner1_ov"
)

makeLearner(
   learnerobject = Learner2, learnername = "Learner2", trigger = "Learner_Create2", selectedlearner = "Learner_Learner2",
   learnerparamoutput = "Learner2_tab", learnerovoutput = "Learner2_ov"
)

makeLearner(
   learnerobject = Learner3, learnername = "Learner3", trigger = "Learner_Create3", selectedlearner = "Learner_Learner3",
   learnerparamoutput = "Learner3_tab", learnerovoutput = "Learner3_ov"
)

makeLearner(
   learnerobject = Learner4, learnername = "Learner4", trigger = "Learner_Create4", selectedlearner = "Learner_Learner4",
   learnerparamoutput = "Learner4_tab", learnerovoutput = "Learner4_ov"
)

makeLearner(
   learnerobject = Learner5, learnername = "Learner5", trigger = "Learner_Create5", selectedlearner = "Learner_Learner5",
   learnerparamoutput = "Learner5_tab", learnerovoutput = "Learner5_ov"
)

makeLearner(
   learnerobject = Learner6, learnername = "Learner6", trigger = "Learner_Create6", selectedlearner = "Learner_Learner6",
   learnerparamoutput = "Learner6_tab", learnerovoutput = "Learner6_ov"
)

makeLearner(
   learnerobject = Learner7, learnername = "Learner7", trigger = "Learner_Create7", selectedlearner = "Learner_Learner7",
   learnerparamoutput = "Learner7_tab", learnerovoutput = "Learner7_ov"
)

# reset Learners when Task changes
observeEvent(currenttask$task, {
   Learner1$Learner <- NULL
   Learner2$Learner <- NULL
   Learner3$Learner <- NULL
   Learner4$Learner <- NULL
   Learner5$Learner <- NULL
   Learner6$Learner <- NULL
   Learner7$Learner <- NULL
   LearnerMeta$Learner_Avail <- NULL
})
