# use of algorithms:
# rpart for decision trees: https://cran.r-project.org/web/packages/rpart/vignettes/longintro.pdf
# ranger for random forests: https://cran.r-project.org/web/packages/ranger/ranger.pdf
#


LearnerMeta <- reactiveValues(Count = 1, learner_choice = NULL, Learner_Avail = NULL)
Learner1 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL)
Learner2 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL)
Learner3 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL)
Learner4 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL)
Learner5 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL)
Learner6 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL)
Learner7 <- reactiveValues(Learner = NULL, Overview = NULL, Params = list(), Predict_Type = NULL, Hash = NULL)

observe({
   if (!is.null(currenttask$task) && currenttask$task$task_type == "classif") {
      LearnerMeta$learner_choice <- c("logistic regression" = "classif.log_reg", "random forest" = "classif.ranger", "decision tree" = "classif.rpart",
                                      "support vector machine" = "classif.svm")
   }
   else if (!is.null(currenttask$task) && currenttask$task$task_type == "regr") {
      LearnerMeta$learner_choice <- c("linear regression" = "regr.lm", "random forest" = "regr.ranger", "decision tree" = "regr.rpart",
                                      "support vector machine" = "regr.svm")
   }
})

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

getCurrentParams <- function(learnerobject) {
   selectedParams <- character(0)
   for (i in names(learnerobject$Learner$param_set$values)) {
      selectedParams <- paste(c(selectedParams, paste(i, learnerobject$Learner$param_set$values[[i]],
                                                      sep = ": ")), collapse = ", ")
   }
  return(selectedParams)
}

getLearnerOverview <- function(learnerobject) {
   learnerobject$Predict_Type <- learnerobject$Learner$predict_type
   overview <- list(
      "name" = names(which(possiblelearners == learnerobject$Learner$id)),
      "predict type" = learnerobject$Predict_Type,
      "params" = getCurrentParams(learnerobject = learnerobject),
      "predit types" = learnerobject$Learner$predict_types,
      "properties" = learnerobject$Learner$properties,
      "feature types" = learnerobject$Learner$feature_types
   )
   return(overview)
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
                addOverviewLineLearner("Current Parameter: ", paste(learnerobject$Overview[[3]], collapse = ", "))
                #addOverviewLineLearner("Supported Predict Types: ", paste(learnerobject$Overview[[4]], collapse = ", "))
                ),
         column(6,
                addOverviewLineLearner("Supported Predict Types: ", paste(learnerobject$Overview[[4]], collapse = ", ")),
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

# for svm to restrict parameter choice for now (e1071 drops unused params automatically, but confusing for newbies and users)
# however, now this is not a good expandable solution - just a quick one; otherwise just offer all params and drop unused ones silently
getKernelParams <- function(learnerobject, learnername, selectedkernel) {
   if (selectedkernel == "polynomial") {
      kernelparams <- tagList(
         addNumericParam(id = "gamma", lower = learnerobject$Learner$param_set$params$gamma$lower,
                         upper = learnerobject$Learner$param_set$params$gamma$upper,
                         learnername = learnername, default = learnerobject$Learner$param_set$params$gamma$default),
         addNumericParam(id = "degree", lower = learnerobject$Learner$param_set$params$degree$lower, 
                         upper = learnerobject$Learner$param_set$params$degree$upper,
                         learnername = learnername, default = learnerobject$Learner$param_set$params$degree$default)
      )
      return(kernelparams)
   }
   else if (selectedkernel == "radial" || selectedkernel == "sigmoid") {
      kernelparams <- tagList(
         addNumericParam(id = "gamma", lower = learnerobject$Learner$param_set$params$gamma$lower, upper = learnerobject$Learner$param_set$params$gamma$upper,
                        learnername = learnername, default = learnerobject$Learner$param_set$params$gamma$default)
      )
      return(kernelparams)
   } 
}

getAvailableParams <- function(algorithm, learnerobject) {
   learnerobject$Params <- learnerparams[[algorithm]]
   params <- list()
   for (i in 1:length(learnerobject$Params)) {
      params[[i]] <- learnerobject$Learner$param_set$params[[learnerobject$Params[i]]]
   }
   return(params)
}

makeParamUi <- function(learnerobject, learnername) {
   if (learnerobject$Learner$param_set$is_empty) {
      return(h5("No Parameters available to be set.", style = "text-align: center;"))
   }
   else if (learnerobject$Learner$id == "classif.ranger" || learnerobject$Learner$id == "regr.ranger") {
       params <- getAvailableParams(algorithm = "ranger", learnerobject = learnerobject)

      #TO-DO: Get a better solution - ugly and repetitive
      parameterRangerUi <- tagList(
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
      return(parameterRangerUi)
   }
   else if (learnerobject$Learner$id == "classif.rpart" || learnerobject$Learner$id == "regr.rpart") {
      params <- getAvailableParams(algorithm = "rpart", learnerobject = learnerobject)

      parameterRpartUi <- tagList(
         addNumericParam(id = params[[1]]$id, lower = params[[1]]$lower, upper = params[[1]]$upper, learnername = learnername,
                         default = params[[1]]$default),
         addNumericParam(id = params[[2]]$id, lower = params[[2]]$lower, upper = params[[2]]$upper, learnername = learnername,
                         default = params[[2]]$default),
         addNumericParam(id = params[[3]]$id, lower = params[[3]]$lower, upper = params[[3]]$upper, learnername = learnername,
                         default = params[[3]]$default),
         actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
      )
      return(parameterRpartUi)
   }
   else if (learnerobject$Learner$id == "classif.svm" || learnerobject$Learner$id == "regr.svm") {
      params <- getAvailableParams(algorithm = "supportvm", learnerobject = learnerobject)
      parameterSvmUi <- tagList(
         # sigmoid kernel removed for explanatory reasons
         addFactorParam(id = params[[1]]$id, levels = c("radial", "polynomial", "linear"), learnername = learnername, default = params[[1]]$default),
         addNumericParam(id = params[[2]]$id, lower = params[[2]]$lower, upper = params[[2]]$upper, learnername = learnername,
                         default = params[[2]]$default),
         uiOutput(outputId = paste0(learnername, "KernelParam", "kernel")),
         actionButton(inputId = paste0(learnername, "ChangeParams"), label = "Change Parameters", style = "float: right;")
      )
      return(parameterSvmUi)
      }
}
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

makeLearnerParamTab <- function(learnerobject, learnername) {
   learnerparams <- tagList(
                     wellPanel(
                           fluidRow(
                              column(12,
                                     h5("Learner Parameters", style = "font-weight: bold;"),
                                     makeParamUi(learnerobject = learnerobject, learnername = learnername)
                              )
                           ),
                           hr(style = "border-color: #3e3f3a;"),
                           fluidRow(
                              column(4,
                                     h5("Change Predict Type")
                              ),
                              column(4,
                                     selectInput(inputId = paste0(learnername, "PredictTypeChoice"), label = NULL, choices = learnerobject$Learner$predict_types,
                                                 selected = learnerobject$Learner$predict_type[1])
                              ),
                              column(4,
                                     actionButton(inputId = paste0(learnername, "PredictTypeChange"), label = "Change", style = "float: right;")
                              )
                           )
                        )
   )
   return(learnerparams)
}

# Note: right now, the default value in learner params UI resets when change_params is clicked
# reason: the whole learner tab is one ui element, so when overview changes, the rest gets rerendered and numericInputs do not remember their values
# future To-DO: encapsulate overview and learner parameter ui in two different uis so that the seleted param values also stays consistend in the Learner Parameter UI
makeLearner <- function(learnerobject, learnername, trigger, selectedlearner, learnerparamoutput, learnerovoutput) {

   observeEvent(input[[trigger]], {
      learnerobject$Learner <- mlr_learners$get(input[[selectedlearner]])
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
   })
   
   # kernel params for svm
   observeEvent(input[[paste0(learnername, "Param", "kernel")]], {
      output[[paste0(learnername, "KernelParam", "kernel")]] <- renderUI({
         getKernelParams(learnerobject = learnerobject, learnername = learnername,
                         selectedkernel = input[[paste0(learnername, "Param", "kernel")]])
      })
   })
   
   # # To-Do: get a prettier solution
   observeEvent(input[[paste0(learnername, "ChangeParams")]], {
      for (i in learnerobject$Params) {
         currentinput <- input[[paste0(learnername, "Param", i)]]
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
               learnerobject$Learner$param_set$values[[i]] <- currentinput
               learnerobject$Overview <- getLearnerOverview(learnerobject = learnerobject)
               learnerobject$Hash <- learnerobject$Learner$hash
               }
         }
      }
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
