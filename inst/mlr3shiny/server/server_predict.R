# reactive values for the predict tab
Pred <- reactiveValues(Learner = NULL, Learner_Ov = NULL, New_Data = NULL, Pred = NULL)
# list of trained learners (will be dynamically filled in the training process)
trained_learner_list <- reactiveValues()

## Functions
# learner selection and overview
getLrnsUi <- function() {
  if (is.null(LearnerMeta$Learner_Avail)) {
    ui <- tagList(
      fluidRow(
        column(12,
               h5("No learner has been created yet.")
        )
      )
    )
    return(ui)
  }
  else {
    ui <- tagList(
      fluidRow(
        column(12,
               radioButtons(inputId = "Pred_learner", label = h5("Select the best learner to train on the entire training data:"),
                                  choices = LearnerMeta$Learner_Avail,
                                  selected = character(0))
        )
      )
    )
    return(ui)
  }
}

getLrnStatus <- function() {
  if (is.null(Pred$Learner$model)) {
    status <- "[not trained]"
  }
  else {
    status <- "trained"
  }
  return(status)
}

# learner overview
createPredLrnOv <- function() {
  overview <- list(
    Learner <- paste(input$Pred_learner, Pred$Learner$id, sep = " "),
    # from server_Learner
    Params <- getCurrentParams(Pred),
    PredType <- Pred$Learner$predict_type,
    Target <- currenttask$task$target_names,
    Status <- getLrnStatus()
  )
  return(overview)
}

getLrnOverview <- function() {
  if (is.null(Pred$Learner)) {
    ui <- tagList(
      hr(style = "border-color: #3e3f3a;"),
      fluidRow(
        column(12,
               h5("No learner has been selected yet.")
        )
      )
    )
    return(ui)
  }
  else {
    overviewui <- tagList(
      hr(style = "border-color: #3e3f3a;"),
      fluidRow(
        column(12,
               addOverviewLineWf("Learner: ", Pred$Learner_Ov[[1]]),
               #addOverviewLineWf("Parameter: ", Pred$Learner_Ov[[2]]),
               addOverviewLineWf("Predict Type", Pred$Learner_Ov[[3]]),
               addOverviewLineWf("Target:", Pred$Learner_Ov[[4]]),
               addOverviewLineWf("Status:", Pred$Learner_Ov[[5]])
        )
      )
    )
    return(overviewui)
  }
}

getLrnTrainBtn <- function() {
  if (!is.null(Pred$Learner)) {
    lrnbutton <- tagList(
      hr(style = "border-color: #3e3f3a;"),
      fluidRow(
        column(12,
               actionButton(inputId = "Pred_train_learner", label = "Train Learner", style = "float: left;"),
               if (!is.null(Pred$Learner$model)) {
                  downloadButton(outputId  = "Pred_trained_learner", label = "Export learner", style = "float: right;")
               } else {
                 hidden (
                  downloadButton(outputId  = "Pred_trained_learner", label = "Export learner", style = "float: right;")
                  )
                  }
        )
      )
    )
  }
}
getNewDataTbl <- function() {
  if (!is.null(Pred$New_Data)) {
    tabl <- DT::datatable(as.data.table(Pred$New_Data),
                          options = list(scrollX = TRUE,searching = FALSE, bInfo = FALSE, lengthChange = FALSE, scrollY = "150px"))
    return(tabl)
  }
}

getNewPrediction <- function() {
  if (!is.null(Pred$Pred)) {
    tabl <- DT::datatable(as.data.table(Pred$Pred),
                          options = list(scrollX = TRUE,searching = FALSE, bInfo = FALSE, lengthChange = FALSE, scrollY = "150px"))
    return(tabl)
  }
}


## ouputs and observers

#learner selection
output$Predict_learner_choice <- renderUI({
  getLrnsUi()
})

#lerner overview and train button
output$Predict_learner_overview <- renderUI({
  getLrnOverview()
})

output$Predict_learner_train_btn <- renderUI({
  getLrnTrainBtn()
})

# train learner
observeEvent(input$Pred_train_learner, {
    withProgress(message = "Training model on all data", {
      withCallingHandlers(
        tryCatch({
          trained_learner_list[[input$Pred_learner]] <- Pred$Learner$train(currenttask$task)
          Pred$Learner <- trained_learner_list[[input$Pred_learner]]
        }
        , error = errorAlertTrain),
        warning = warningAlert)
      incProgress(0.8)
      show(id = "Pred_trained_learner")
    })
  Pred$Learner_Ov <- createPredLrnOv()
})


# download learner
output$Pred_trained_learner <- downloadHandler(
  filename = function() {
    paste(paste("Trained_Learner", Pred$Learner$id, sep = "_"), ".rds", sep = "")
  },
  content = function(file) {
    saveRDS(Pred$Learner, file = file)
  }
)

# get new data import
observe({
  if (input$Predict_data_type == "csv" || input$Predict_data_type == "txt") {
    filepath <-  input$Predict_data_csv$datapath
    if (!is.null(filepath) && (str_sub(filepath, -4, -1) == ".csv" || (str_sub(filepath, -4, -1) == ".txt" ))) {
      Pred$New_Data <- read.csv(file = filepath, header = input$Predict_data_header,
                                   sep = input$Predict_data_sep, quote = input$Predict_data_quote)
    }
  } else if (input$Predict_data_type == "xlsx") {
     filepath <-  input$Predict_data_xlsx$datapath
        if (!is.null(filepath) && (str_sub(filepath, -5, -1) == ".xlsx" )) {
          predicttibble <- read_excel(path = filepath, col_names = input$Predict_data_header_xlsx,
                                         sheet = input$Predict_data_sheet)
          predictdf <- as.data.frame(predicttibble)
          Pred$New_Data <- modify_at(predictdf,
                                        which(as.character(sapply(predictdf, class)) == "character"),
                                        as.factor)
    }
  }
})

observe({
  toggle(id = "Pred_well_new_data", condition = !is.null(Pred$New_Data))
})

output$Pred_new_data_view <- DT::renderDataTable({
  getNewDataTbl()
})

# make the prediction
observeEvent(input$Predict_predict, {
  if (is.null(Pred$Learner) || is.null(Pred$New_Data)) {
    shinyalert(title = "Predicting Failed",
               text = paste("Please train a learner on the entire training data set and import a new dataset prior to predicting.",
                            "the target value", sep = " "),
               closeOnClickOutside = TRUE, animation = FALSE)
  }
  else {
    withCallingHandlers(
      tryCatch(Pred$Pred <- Pred$Learner$predict_newdata(task = currenttask$task, newdata = Pred$New_Data[, features_to_use$features]),
               error = errorAlertPredictNew),
      warning = warningAlert)
  }
})

observe({
  toggle(id = "Pred_well_prediction", condition = !is.null(Pred$Pred))
})

output$Pred_prediction_view <- DT::renderDataTable({
  getNewPrediction()
})

# download prediction as csv or rds
output$Pred_prediction_download_csv <- downloadHandler(
  filename = function() {
    paste("Prediction_new_data", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(x = as.data.table(Pred$Pred), file = file)
  }
)
output$Pred_prediction_download_rds <- downloadHandler(
  filename = function() {
    paste("Prediction_new_data", ".rds", sep = "")
  },
  content = function(file) {
    saveRDS(object = Pred$Pred, file = file)
  }
)

# reset Learner
resetPredLrn <- function() {
  Pred$Learner <- NULL
  Pred$Learner_Ov <- NULL
}

observeEvent(input$Pred_learner, {
  if (!is.null(trained_learner_list[[input$Pred_learner]])) {
    Pred$Learner <- trained_learner_list[[input$Pred_learner]]
  } else {
    Pred$Learner <- get(input$Pred_learner)$Learner$clone(deep = TRUE)
  }
  Pred$Learner_Ov <- createPredLrnOv()
})

observeEvent(currenttask$task, {
  resetPredLrn()
})
