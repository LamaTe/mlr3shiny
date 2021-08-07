# each Learner has its own reactive values
LearnerMeta <- reactiveValues(Count = 1, learner_choice = NULL, Learner_Avail = NULL, TwoClass = NULL)
Learner1 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL)
Learner2 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL)
Learner3 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL)
Learner4 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL)
Learner5 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL)
Learner6 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL)
Learner7 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL)

# check whether it is a regression or classification and then give possible learners for each
observe({
   if (!is.null(currenttask$task) && currenttask$task$task_type == "classif") {
      basic_choice <- c("decision tree" = "classif.rpart", "random forest" = "classif.ranger", "support vector machine" = "classif.svm", "xgboost" = "classif.xgboost")
      if (currenttask$task$properties == 'multiclass') {
         LearnerMeta$learner_choice <- basic_choice
      }
      else {
         LearnerMeta$TwoClass <- TRUE
         LearnerMeta$learner_choice <- c(basic_choice,  "logistic regression" = "classif.log_reg")
      }
   }
   else if (!is.null(currenttask$task) && currenttask$task$task_type == "regr") {
      LearnerMeta$learner_choice <- c("decision tree" = "regr.rpart", "linear regression" = "regr.lm", "random forest" = "regr.ranger",
                                      "support vector machine" = "regr.svm", "xgboost" = "regr.xgboost")
   }
})

# let's user add another learner object
addLearner <- function(LearnNumber){
  tagList(
    fluidRow(
      column(3,
             h5(paste("Learner", (LearnNumber), sep = " "))
      ),
      column(6,
             selectInput(inputId = paste0("Learner_Learner", LearnNumber), label = NULL, choices = LearnerMeta$learner_choice, selected = "decision tree")
      ),
      column(3,
             actionButton(inputId = paste0("Learner_Create", LearnNumber), label = "Go", icon = icon("filter"),
                          style = "float:right;")
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
          "2" = {output$Learner_other_Learner2 <- renderUI({
                  addLearner(currentcount)})},
          "3" = {output$Learner_other_Learner3 <- renderUI({
                  addLearner(currentcount)})},
          "4" = {output$Learner_other_Learner4 <- renderUI({
                  addLearner(currentcount)})},
          "5" = {output$Learner_other_Learner5 <- renderUI({
             addLearner(currentcount)})},
          "6" = {output$Learner_other_Learner6 <- renderUI({
             addLearner(currentcount)})},
          "7" = {output$Learner_other_Learner7 <- renderUI({
             addLearner(currentcount)})},
          {shinyalert(title = "Maximum Reached",
                      text = "Currently a maximum of 7 learners at the same time is supported.",
                      animation = FALSE, closeOnClickOutside = TRUE)}
          )
})

# only show learner panels when at least one is created otherwise give user info that no learner has been created yet
observe({
   checkcondition <- is.null(c(Learner1$Learner, Learner2$Learner, Learner3$Learner, Learner4$Learner,
                               Learner5$Learner, Learner6$Learner, Learner7$Learner))
   toggle(id = "Learner_NA", condition = checkcondition)
   toggle(id = "Learner_Learners", condition = !checkcondition)
})

hideorshowTab <- function(learnerobject, target) {
   if (is.null(learnerobject)) {
      hideTab(inputId = "Learner_Learners_Tab", target = target)
   }
   else {
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
                                                      sep = ": ")), collapse = ", ")
   }
  return(selectedParams)
}

getLearnerOverview <- function(learnerobject) {
   overview <- list(
      "name" = getDisplayLearnerName(learnerobject),
      "predict type" = learnerobject$Learner$predict_type,
      # do not include params as svm still remembers parameter set for radial kernel such as gamma, even though it is not
      # used in linear kernel -> confusing for user
      "params" = getCurrentParams(learnerobject = learnerobject),
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

addOverviewLineLearner = function(title, body) {
   fluidRow(
      column(6, h5(title)),
      column(6, h5(body))
   )
}

makeOverviewUi <- function(learnerobject) {
   overviewUi <- tagList(
      fluidRow(
         column(6,
                addOverviewLineLearner("Algorithm: ", learnerobject$Overview[[1]]),
                addOverviewLineLearner("Current Predict Type: ", learnerobject$Overview[[2]]),
                #addOverviewLineLearner("Current Parameter: ", paste(learnerobject$Overview[[3]], collapse = ", "))
                addOverviewLineLearner("Supported Predict Types: ", paste(learnerobject$Overview[[4]], collapse = ", "))
                ),
         column(6,
                #addOverviewLineLearner("Supported Predict Types: ", paste(learnerobject$Overview[[4]], collapse = ", ")),
                addOverviewLineLearner("Properties: ", paste(learnerobject$Overview[[5]], collapse = ", ")),
                addOverviewLineLearner("Supported Feature Types: ", paste(learnerobject$Overview[[6]], collapse = ", "))
                )
      )
   )
   return(overviewUi)
}

## Learner params
addNumericParam <- function(id, lower, upper, learnername, default) {
   fluidRow(
      column(3,
             h5(id)
      ),
      column(3,
             h5(paste("Lower:", lower, sep = " "))
      ),
      column(3,
             h5(paste("Upper:", upper, sep = " "))
      ),
      column(3,
             numericInput(inputId = paste0(learnername, "Param", id), label = NULL,
                          value = default, min = lower, max = upper)
      )

   )
}



addFactorParam <- function(id, levels, learnername, default) {
   fluidRow(
      column(3,
             h5(id)
      ),
      column(6,
             h5(paste("Levels:", paste(levels, collapse = ", "), sep = " "))
      ),
      column(3,
             selectInput(inputId = paste0(learnername, "Param", id), label = NULL, choices = levels, selected = default)
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
         addNumericParam(id = "gamma", lower = learnerobject$Learner$param_set$params$gamma$lower,
                         upper = learnerobject$Learner$param_set$params$gamma$upper,
                         learnername = learnername, default = learnerobject$Learner$param_set$params$gamma$default),
         addNumericParam(id = "degree", lower = learnerobject$Learner$param_set$params$degree$lower,
                         upper = learnerobject$Learner$param_set$params$degree$upper,
                         learnername = learnername, default = learnerobject$Learner$param_set$params$degree$default)
      )
      # update learnerobject$Params so that only the hyperparams are set that are actually available
      learnerobject$Params <- c('kernel', 'cost', 'gamma', 'degree')
      return(kernelparams)
   }
   else if (selectedkernel == "radial" || selectedkernel == "sigmoid") {
      kernelparams <- tagList(
         addNumericParam(id = "gamma", lower = learnerobject$Learner$param_set$params$gamma$lower, upper = learnerobject$Learner$param_set$params$gamma$upper,
                        learnername = learnername, default = learnerobject$Learner$param_set$params$gamma$default)
      )
      learnerobject$Params <- c('kernel', 'cost', 'gamma')
      return(kernelparams)
   }
   else{
      learnerobject$Params <- c('kernel', 'cost')
      return(NULL)
   }
}

getAvailableParams <- function(algorithm, learnerobject) {
   # Input: Selected algorithm as defined in global, learnerobject that refers to an mlr3 learner
   # Selects available parameters for given algorithm from global and gets parameter details through mlr3
   # Implicitly assigns available hyperparameter to learner$Params for later reference when setting hyperparams
   # Output: list of parameters with id, lower and upper levels, defaults
   learnerobject$Params <- learnerparams[[algorithm]]
   params <- list()
   stripped_id <- str_extract(learnerobject$Learner$id, "^\\w+\\.\\w+")
   for (i in 1:length(learnerobject$Params)) {
      # concatenating the learner id (e.g. "classif.rpart") with a . and the actual parameter
      # this is required because the graph learner stores all parameters in this format 
      # example: "classif.rpart.maxdepth"
      params[[i]] <- learnerobject$Learner$param_set$params[[paste0(stripped_id, ".", learnerobject$Params[i])]]
   }
   if (grepl("threshold", learnerobject$Learner$id)) {
      params[[length(params)+1]] <- learnerobject$Learner$param_set$params[["threshold.thresholds"]]
   }
   str(params)
   return(params)
}

# display a button to activate binary thresholding when the task is twoclass
# and the predict_type is prob
makeThreshHoldButton <- function(learnerobject, learnername) {
   if (currenttask$task$properties == "twoclass") {
      if (learnerobject$Learner$predict_type == "prob") {
         threshold <- actionButton(inputId = paste0(learnername, "ThresholdSwitch"), label = "Use Binary-Threshold", style = "float: left;")
      } else {
         threshold <- disabled(
            actionButton(inputId = paste0(learnername, "ThresholdSwitch"), label = "Use Binary-Threshold", style = "float: left;")
         )
      }
      return(threshold)
   }
}

makeThresholdUi <- function(learnerobject, learnername) {
   
}

# define the parameter settings UI for each learner depending on the selected algorithm
makeParamUi <- function(learnerobject, learnername) {
   # Input: mlr3 learner object, name of currently selected learner (learner1, learner2 etc)
   # Creates layout for hyperparameters as a taglist depending on the algorithm
   # Returns: taglist of inputs for Learner hyperparameters
   parameterui <- NULL

   if (learnerobject$Learner$param_set$is_empty) {
      return(h5("No Parameters available to be set.", style = "text-align: center;"))
   }
   else if (grepl("ranger", learnerobject$Learner$id, fixed = TRUE)) {
      params <- getAvailableParams(algorithm = "ranger", learnerobject = learnerobject)
      #TO-DO: Get a better solution - ugly and repetitive
      parameterui <- tagList(
         #num.trees
         addNumericParam(id = params[[1]]$id, lower = params[[1]]$lower, upper = params[[1]]$upper, learnername = learnername,
                       default = params[[1]]$default),
         #mtry upper needs to be restricted to number of features since mlr ships with Inf as upper value
         addNumericParam(id = params[[2]]$id, lower = params[[2]]$lower, upper = length(currenttask$task$feature_names), learnername = learnername,
                       default = params[[2]]$default),
         #min.node.size
         addNumericParam(id = params[[3]]$id, lower = params[[3]]$lower, upper = params[[3]]$upper, learnername = learnername,
                       default = params[[3]]$default),
         actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
      )
   }
   else if (learnerobject$Learner$id == "classif.rpart" || learnerobject$Learner$id == "regr.rpart") {
      params <- getAvailableParams(algorithm = "rpart", learnerobject = learnerobject)
      parameterui <- tagList(
         addNumericParam(id = params[[1]]$id, lower = params[[1]]$lower, upper = params[[1]]$upper, learnername = learnername,
                         default = params[[1]]$default),
         addNumericParam(id = params[[2]]$id, lower = params[[2]]$lower, upper = params[[2]]$upper, learnername = learnername,
                         default = params[[2]]$default),
         addNumericParam(id = params[[3]]$id, lower = params[[3]]$lower, upper = params[[3]]$upper, learnername = learnername,
                         default = params[[3]]$default),
         actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
      )
   }
   else if (learnerobject$Learner$id == "classif.svm" || learnerobject$Learner$id == "regr.svm") {
      params <- getAvailableParams(algorithm = "supportvm", learnerobject = learnerobject)
      parameterui <- tagList(
         # sigmoid kernel removed for explanatory reasons
         addFactorParam(id = params[[1]]$id, levels = c("radial", "polynomial", "linear"), learnername = learnername, default = params[[1]]$default),
         addNumericParam(id = params[[2]]$id, lower = params[[2]]$lower, upper = params[[2]]$upper, learnername = learnername,
                         default = params[[2]]$default),
         uiOutput(outputId = paste0(learnername, "KernelParam", "kernel")), # depending on selected kernel, different hyperparameters are available
         actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
      )
      }
   else if (learnerobject$Learner$id == "classif.xgboost" || learnerobject$Learner$id == "regr.xgboost") {
      params <- getAvailableParams(algorithm = "xgboost", learnerobject = learnerobject)
      parameterui <- tagList(
         addNumericParam(id = params[[1]]$id, lower = params[[1]]$lower, upper = params[[1]]$upper, learnername = learnername, default = params[[1]]$default),
         addNumericParam(id = params[[2]]$id, lower = params[[2]]$lower, upper = params[[2]]$upper, learnername = learnername, default = params[[2]]$default),
         addNumericParam(id = params[[3]]$id, lower = params[[3]]$lower, upper = params[[3]]$upper, learnername = learnername, default = params[[3]]$default),
         addNumericParam(id = params[[4]]$id, lower = params[[4]]$lower, upper = params[[4]]$upper, learnername = learnername, default = params[[4]]$default),
         addFactorParam(id = params[[5]]$id, levels = c("gblinear", "gbtree","dart"), learnername = learnername, default = params[[5]]$default),
         actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
      )
   }
   if (grepl("threshold", learnerobject$Learner$id)) {
      addNumericParam(id = params[[length(params)]]$id, lower = params[[length(params)]]$lower, upper = params[[length(params)]]$upper, learnername = learnername, default = params[[length(params)]]$default)
   }
   return(parameterui)
}

# make the overview for each learner
makeLearnerOvTab <- function(learnerobject) {
   learnerov <- tagList(
      wellPanel(
         fluidRow(
            column(12,
                   h5("Learner Information", style = "font-weight: bold;"),
                   makeOverviewUi(learnerobject = learnerobject)
            )
         )
      )
   )
}

# place overview and learner together in a tab
makeLearnerParamTab <- function(learnerobject, learnername) {
   learnerparams <- tagList(
                     wellPanel(
                           fluidRow(
                              column(12,
                                     h5("Learner Parameters", style = "font-weight: bold;"),
                                     makeParamUi(learnerobject = learnerobject, learnername = learnername),
                                     makeThreshHoldButton(learnerobject = learnerobject, learnername = learnername),
                                     makeThresholdUi(learnerobject = learnerobject, learnername = learnername)
                              ),
                              
                           ),
                           hr(style = "border-color: #3e3f3a;"),
                           fluidRow(
                              column(4,
                                     h5("Change Predict Type")
                              ),
                              column(4,
                                     selectInput(inputId = paste0(learnername, "PredictTypeChoice"), label = NULL, choices =learnerobject$Learner$predict_types,
                                                 selected = learnerobject$Learner$predict_type)
                              ),
                              column(4,
                                     actionButton(inputId = paste0(learnername, "PredictTypeChange"), label = "Change", style = "float: right;")
                              )
                           )
                        )
   )
   return(learnerparams)
}


# add observers and others to generate the tabs depending on the needs of the user
makeLearner <- function(learnerobject, learnername, trigger, selectedlearner, learnerparamoutput, learnerovoutput) {

   observeEvent(input[[trigger]], {
      if (currenttask$task$properties == "twoclass") {
         learnerobject$Learner <- as_learner(po("learner", lrn(input[[selectedlearner]])) %>>% po("threshold"))
      } else {
         learnerobject$Learner <- as_learner(po("learner", lrn(input[[selectedlearner]])))
      }
      # learnerobject$Learner <- mlr_learners$get(input[[selectedlearner]])
      LearnerMeta$Learner_Avail <- unique(sort(c(LearnerMeta$Learner_Avail, learnername)))
      learnerobject$Hash <- learnerobject$Learner$hash
      output[[learnerparamoutput]] <- renderUI({
         makeLearnerParamTab(learnerobject = learnerobject, learnername = learnername)
      })
      output[[learnerovoutput]] <- renderUI({
         makeLearnerOvTab(learnerobject = learnerobject)
      })
   })

   observe({if (!is.null(learnerobject$Learner)) {
      learnerobject$Overview <- getLearnerOverview(learnerobject = learnerobject)}
      learnerobject$Hash <- learnerobject$Learner$hash
   })

   # missing reactivity with R6-classes requires another execution of getLearnerOverview
   observeEvent(input[[paste0(learnername, "PredictTypeChange")]], {
      learnerobject$Learner$predict_type <- input[[paste0(learnername, "PredictTypeChoice")]]
      learnerobject$Overview <- getLearnerOverview(learnerobject = learnerobject)
      learnerobject$Hash <- learnerobject$Learner$hash
      if (learnerobject$Learner$predict_type == "prob"){
         enable(paste0(learnername, "ThresholdSwitch"))
      } else {
         disable(paste0(learnername, "ThresholdSwitch"))
      } 
   })

   # kernel params for svm
   output[[paste0(learnername, "KernelParam", "kernel")]] <- renderUI({

      # if statement necessary to avoid strange shiny internal warning about empty condition when generating HTML
      if(input[[selectedlearner]] %in% c('classif.svm', 'regr.svm')) {
         getKernelParams(learnerobject = learnerobject, learnername = learnername,
                          selectedkernel = input[[paste0(learnername, "Param", "kernel")]])
         }
   })

   # # To-Do: get a prettier solution
   observeEvent(input[[paste0(learnername, "ChangeParams")]], {
      paramlist <- list()
      for (i in learnerobject$Params) {
         currentinput <- input[[paste0(learnername, "Param", i)]]
         # validate input value
         if (!is.na(currentinput) && !is.null(currentinput)) {
            if ((!is.na(learnerobject$Learner$param_set$params[[i]]$upper) &&
                 currentinput > learnerobject$Learner$param_set$params[[i]]$upper) ||
                (!is.na(learnerobject$Learner$param_set$params[[i]]$lower) &&
                currentinput < learnerobject$Learner$param_set$params[[i]]$lower) ||
                (i == "mtry" && currentinput > length(currenttask$task$feature_names))) {
                  shinyalert(title = "Invalid Parameter Input",
                             text = "It seems that you tried to set a parameter that is not within its parameter range. Please set a valid value.",
                             animation = FALSE, closeOnClickOutside = TRUE)
            }
            else {
               paramlist[[i]] <- currentinput
               }
         }
      }

      # explicit defaults for svm type to be used
      if (learnerobject$Learner$id == 'classif.svm') {
         paramlist[['type']] <- 'C-classification'
      }
      else if (learnerobject$Learner$id == 'regr.svm') {
         paramlist[['type']] <- 'eps-regression'
      }

      learnerobject$Learner$param_set$values <- paramlist # update hyperparameter values of current learner
      #learnerobject$Overview <- getLearnerOverview(learnerobject = learnerobject)
      learnerobject$Hash <- learnerobject$Learner$hash
   })
}


# TO-DO: ugly code, think of better solution eg add simple for loop
makeLearner(learnerobject = Learner1, learnername = "Learner1", trigger = "Learner_Create1", selectedlearner = "Learner_Learner1",
            learnerparamoutput = "Learner1_tab", learnerovoutput = "Learner1_ov")

makeLearner(learnerobject = Learner2, learnername = "Learner2", trigger = "Learner_Create2", selectedlearner = "Learner_Learner2",
            learnerparamoutput = "Learner2_tab", learnerovoutput = "Learner2_ov")

makeLearner(learnerobject = Learner3, learnername = "Learner3", trigger = "Learner_Create3", selectedlearner = "Learner_Learner3",
            learnerparamoutput = "Learner3_tab", learnerovoutput = "Learner3_ov")

makeLearner(learnerobject = Learner4, learnername = "Learner4", trigger = "Learner_Create4", selectedlearner = "Learner_Learner4",
            learnerparamoutput = "Learner4_tab", learnerovoutput = "Learner4_ov")

makeLearner(learnerobject = Learner5, learnername = "Learner5", trigger = "Learner_Create5", selectedlearner = "Learner_Learner5",
            learnerparamoutput = "Learner5_tab", learnerovoutput = "Learner5_ov")

makeLearner(learnerobject = Learner6, learnername = "Learner6", trigger = "Learner_Create6", selectedlearner = "Learner_Learner6",
            learnerparamoutput = "Learner6_tab", learnerovoutput = "Learner6_ov")

makeLearner(learnerobject = Learner7, learnername = "Learner7", trigger = "Learner_Create7", selectedlearner = "Learner_Learner7",
            learnerparamoutput = "Learner7_tab", learnerovoutput = "Learner7_ov")

# reset Learners when Task changes
observeEvent(currenttask$task, {
   Learner1$Learner = NULL
   Learner2$Learner = NULL
   Learner3$Learner = NULL
   Learner4$Learner = NULL
   Learner5$Learner = NULL
   Learner6$Learner = NULL
   Learner7$Learner = NULL
   LearnerMeta$Learner_Avail <- NULL
})
