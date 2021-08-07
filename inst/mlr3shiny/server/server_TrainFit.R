### Basic Workflow reactive values
Help <- reactiveValues(Tracker = 1)
Wf <- reactiveValues(Current_Learner = NULL, Overview = NULL, State = NULL, TrainIds = NULL, TestIds = NULL,
                     Pred_Test = NULL, Pred_Train = NULL, Perf_Test = NULL, Perf_Train = NULL)

## Functions
# get Learner choices
getLearnChoicesUI <- function() {
  if (is.null(LearnerMeta$Learner_Avail)) {
    ui <- tagList(
      column(12,
             h5("No learner has been created yet in step 3.")
      )
    )
    return(ui)
  }
  else {
    ui <- tagList(
        column(6,
               radioButtons(inputId = "TrainFit_learner", label = h5("Select a new learner:"), choices = LearnerMeta$Learner_Avail,
                            selected = character(0))
        )
      )
    return(ui)
  }
}
# get Basic Workflow data
getWfPerfTest <- function() {
  if (is.null(Wf$Perf_Test)) {
    return("[not scored]")
  }
  else {
    wfPerf <- character(0)
    for (i in names(Wf$Perf_Test)) {
      wfPerf <- paste(c(wfPerf, paste(i, round(Wf$Perf_Test[[i]], 3), sep = ": ")), collapse = ", ")
    }
    return(wfPerf)
  }
}

getWfPerfTrain <- function() {
  if (is.null(Wf$Perf_Train)) {
    return("[not scored]")
  }
  else {
    wfPerf <- character(0)
    for (i in names(Wf$Perf_Train)) {
      wfPerf <- paste(c(wfPerf, paste(i, round(Wf$Perf_Train[[i]], 3), sep = ": ")), collapse = ", ")
    }
    return(wfPerf)
  }
}

getLrnModel <- function() {
  if (is.null(Wf$Current_Learner$model)) {
    return("[missing]")
  }
  else {
    return(str_replace(Wf$Current_Learner$id, "^(.+?)\\.",""))
  }
}

getWfState <- function() {
  if (is.null(Wf$Current_Learner$model)) {
    Wf$State <- "defined"
  }
  else if (is.null(Wf$Pred_Test)) {
    Wf$State <- "trained"
  }
  else if (is.null(Wf$Perf_Test)) {
    Wf$State <- "predicted"
  }
  return(Wf$State)
}

createWfOverview <- function() {
  overview <- list(
    Task <- currenttask$task$id,
    Learner <- paste(input$TrainFit_learner, Wf$Current_Learner$id, sep = " "),
    "State of Workflow" <- getWfState(),
    Model <- getLrnModel(),
    "Performance Train Data" <- getWfPerfTrain(),
    "Performance Test Data" <- getWfPerfTest()
  )
  return(overview)
}

# create Basic Workflow overview ui
addOverviewLineWf = function(title, body) {
  fluidRow(
    column(6, h5(title)),
    column(6, h5(body))
  )
}

getWfOverviewUi <- function() {
  if (is.null(Wf$Current_Learner)) {
    return(h5("The basic workflow has not been started yet. Please define a learner in step 3 and select it."))
  }
  else {
    overviewUi <- tagList(
      fluidRow(
        column(12,
               h5("Basic Workflow Overview", style = "font-weight: bold;"),
               addOverviewLineWf("Task: ", Wf$Overview[[1]]),
               addOverviewLineWf("Learner: ", Wf$Overview[[2]]),
               addOverviewLineWf("State of Workflow: ", Wf$Overview[[3]]),
               addOverviewLineWf("Model: ", Wf$Overview[[4]]),
               addOverviewLineWf("Performance Training Set: ", Wf$Overview[[5]]),
               addOverviewLineWf("Performance Test Set: ", Wf$Overview[[6]])
        )
      )
    )
    return(overviewUi)
  }
}


# validate and train model
trainModel <- function(inputsplit, inputseed) {
  withProgress(message = "Training model", {
    set.seed(inputseed)
    Wf$TrainIds <- sample(currenttask$task$row_ids, currenttask$task$nrow * (inputsplit / 100))
    Wf$TestIds <- setdiff(currenttask$task$row_ids, Wf$TrainIds)
    incProgress(0.3)
    withCallingHandlers(
      tryCatch({
          Wf$Current_Learner$train(task = currenttask$task, row_ids = Wf$TrainIds)
        },
        error = errorAlertTrain,
        warning = warningAlert
      )
    )
    incProgress(0.5)
    Wf$Overview <- createWfOverview()
    #toggle(id = "TrainPred_model_download", condition = (!is.null(Exp$Model)))
  })
}

validateSplit <- function(inputsplit, inputseed) {
  if (inputsplit == 0 || inputsplit == 100) {
    shinyalert(title = "Invalid Parameter Input",
               text = paste("The data split that you want to perform is not supported.",
                            "If you want to train your model on the entire training dataset, please got to the 'predict'-tab, instead.",
                            sep = " "),
               animation = FALSE, closeOnClickOutside = TRUE)
  }
  else {
    trainModel(inputseed = inputseed, inputsplit = inputsplit)
  }
}

# create Train model ui
getTrainUi <- function() {
  if (is.null(Wf$Current_Learner)) {
    return(h5("No learner has been selected yet."))
  }
  else {
    trainui <- tagList(
      fluidRow(
        column(12,
               h5(paste("Select percentage of training data for model training and evaluation.",
                        "Set a seed to reproduce the random partitioning anytime.",
                        "The default value to start sampling the training data from is 42.", sep = " "))
        )
      ),
      fluidRow(
        column(3, numericInput(inputId = "TrainFit_seed", label = NULL, value = 42)
        ),
        column(9, sliderInput(inputId = "TrainFit_input_split", label = NULL, min = 0, max = 100, value = 80)
        )
      ),
      fluidRow(
        column(12,
               actionButton(inputId = "TrainFit_train_model", label = "Train model", style = "float: left;")
        )
      )
    )
    return(trainui)
  }
}

# create predict target ui
getPredictUi <- function(Wfstate) {
  if (is.null(Wfstate) || Wfstate == "defined") {
    return(h5("The learner model has not been trained yet."))
  }
  else {
    predictui <- tagList(
      fluidRow(
        column(12,
               h5(paste("Use the trained model to predict the target values on the remaining test data of the training-test split",
                        "as well as on the data partition the model was trained on.", sep = " "))
        )
      ),
      fluidRow(
        column(12,
               actionButton(inputId = "TrainFit_predict_data", label = "Predict target", style = "float: left;")
        )
      )
    )
    return(predictui)
  }
}

# create score ui
getScoreUi <- function(Wfstate){
  if (is.null(Wfstate) || Wfstate == "defined" || Wfstate == "trained") {
    return(h5("A prediction has not been computed yet."))
  }
  else {
    scoreui <- tagList(
      fluidRow(
        column(6,
               selectizeInput(inputId = "TrainFit_select_measure", label = NULL,
                              choices = get_msrs(currenttask$task, Wf$Current_Learner, avail_msrs, msr_translations),
                              options = list(
                                placeholder = 'Nothing selected',
                                onInitialize = I('function() { this.setValue(""); }')
                              ),
                              multiple = TRUE)
        ),
        column(6,
               actionButton(inputId = "TrainFit_score_perf", label = "Score", style = "float: right;")
        )
      )
    )
    return(scoreui)
  }
}

getPredTable <- function(currentpred) {
  if (!is.null(currentpred)) {
    tabl <- DT::datatable(as.data.table(currentpred),
                          options = list(scrollX = TRUE,searching = FALSE, bInfo = FALSE, lengthChange = FALSE))
    return(tabl)
  }
}

# reset Workflow
resetWf <- function() {
  Wf$Current_Learner <- NULL
  Wf$TrainIds <- NULL
  Wf$ValidIds <- NULL
  Wf$Pred_Test <- NULL
  Wf$Pred_Train <- NULL
  Wf$Perf_Test <- NULL
  Wf$Perf_Train <- NULL
  Wf$State <- NULL
}
##


## Observers
# select Learner when available
output$TrainFit_learner_selection <- renderUI({
  getLearnChoicesUI()
})

# Create and update Workflow Overview
output$TrainFit_overview <- renderUI({
  getWfOverviewUi()
})


# show train model ui
output$TrainFit_train <- renderUI({
  getTrainUi()
})

# ensure data split is adequate
observeEvent(input$TrainFit_train_model, {
  validateSplit(inputsplit = input$TrainFit_input_split, inputseed = input$TrainFit_seed)
})



# show predict target ui
output$TrainFit_predict <- renderUI({
  getPredictUi(getWfState())
})

# predict target
observeEvent(input$TrainFit_predict_data, {
  withCallingHandlers(
    tryCatch({
      Wf$Pred_Train <- Wf$Current_Learner$predict(task = currenttask$task, row_ids = Wf$TrainIds)
      Wf$Pred_Test <- Wf$Current_Learner$predict(task = currenttask$task, row_ids = Wf$TestIds)},
      error = errorAlertPredict
    ),
    warning = warningAlert
  )
  Wf$Overview <- createWfOverview()
})

# show Train/Test prediction tables
output$TrainFit_pred_view_test <- DT::renderDataTable({
  getPredTable(Wf$Pred_Test)
})

output$TrainFit_pred_view_train <- DT::renderDataTable({
  getPredTable(Wf$Pred_Train)
})

# show prediction data tables
observe({
  toggle(id = "TrainFit_well_test", condition = !is.null(Wf$Pred_Test))
  toggle(id = "TrainFit_well_train", condition = !is.null(Wf$Pred_Train))
})



# show Score ui
output$TrainFit_score <- renderUI({
  getScoreUi(Wfstate = getWfState())
})
# compute and show scored train/test performance
observeEvent(input$TrainFit_score_perf, {
  measures <- msrs(c(input$TrainFit_select_measure))
  Wf$Perf_Train <- Wf$Pred_Train$score(measures)
  Wf$Perf_Test <- Wf$Pred_Test$score(measures)
  Wf$Overview <- createWfOverview()
})



# set current learner
observeEvent(input$TrainFit_learner, {
  resetWf()
  Wf$Current_Learner <- get(input$TrainFit_learner)$Learner$clone(deep = TRUE)
  Wf$Overview <- createWfOverview()
})

# reset Workflow when task changes
observeEvent(currenttask$task, {
  resetWf()

})

### Use Resampling
observeEvent(input$TrainFit_resample, {
  toggle(id = "TrainFit_Basic")
  toggle(id = "TrainFit_Resample")
  Help$Tracker <- 2
})

observeEvent(input$TrainFit_Base, {
  toggle(id = "TrainFit_Resample")
  toggle(id = "TrainFit_Basic")
  Help$Tracker <- 1
})
