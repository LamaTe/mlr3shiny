# reactive values for the predict tab
Pred <- reactiveValues(Learner = NULL, Learner_Ov = NULL, New_Data = NULL, Pred = NULL)

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
               addOverviewLineWf("Predict Type", Pred$Learner_Ov[[2]]),
               addOverviewLineWf("Target:", Pred$Learner_Ov[[3]]),
               addOverviewLineWf("Status:", Pred$Learner_Ov[[4]])
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

getLrnCodegenBtn <- function() {
  if (!is.null(Pred$Learner)) {
    lrnbutton <- tagList(
      hr(style = "border-color: #3e3f3a;"),
      fluidRow(
        column(12,
            hidden(
            actionButton(inputId = "Pred_codegen", label = "Show the Code", icon = icon("code"), style = "stretch", width = "100%")
            )
        )
      )
    )
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

# code-generation button
output$Predict_codegen_btn <- renderUI({
  getLrnCodegenBtn()
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
      show(id = "Pred_codegen")
    })
  Pred$Learner_Ov <- createPredLrnOv()
})

# display generated code when the button is pressed
observeEvent(input$Pred_codegen, {
  showModal(
    modalDialog(
      title = h2("Code Generation", style = "text-align: center;"),
      h4("## Task Creation"),
      HTML(get_task_code(currenttask$task)),
      h4("## Learner Creation"),
      HTML(get_learner_code(Pred$Learner)),
      h4("## Training"),
      HTML(get_training_code()),
      h4("## Scoring"),
      HTML(get_score_code(currenttask$task, Pred$Learner)),
      h4("## Resampling"),
      HTML(get_resampling_code()),
      h4("## Final Training and Predict"),
      HTML(get_final_training_code(currenttask$task, Pred$Learner)),
      easyClose = TRUE,
      footer = div(style = "display:inline-block;width:100%;text-align: center;",
                   modalButton('OK')))
      )
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
  } else if (input$Predict_data_type == "spss") {
    filepath <-  input$Predict_data_spss$datapath
    if (!is.null(filepath) && (str_sub(filepath, -4, -1) == ".sav" | str_sub(filepath, -4, -1) == ".por") ) {
      predicttibble <- read_spss(file = filepath)
      predictdf <- as.data.frame(predicttibble)
      predictdf <- as_factor(predictdf)
      Pred$New_Data <- modify_at(predictdf,
                                  which(as.character(sapply(predictdf, class)) == "character"),
                                  as.factor)
    }
  } else if (input$Predict_data_type == "sas") {
    filepath <-  input$Predict_data_sas$datapath
    if (!is.null(filepath) && (str_sub(filepath, -9, -1) == ".sas7bdat") ) {
      predicttibble <- read_sas(data_file = filepath)
      predictdf <- as.data.frame(predicttibble)
      Pred$New_Data <- modify_at(predictdf,
                                  which(as.character(sapply(predictdf, class)) == "character"),
                                  as.factor)
    }
  } else if (input$Predict_data_type == "stata") {
    filepath <-  input$Predict_data_stata$datapath
    if (!is.null(filepath) && (str_sub(filepath, -4, -1) == ".dta") ) {
      predicttibble <- read_stata(file = filepath)
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
  reset_trained_learner_list()
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
quote_seperator <- NULL
## Functionality for code generation
# get code-block for task generation
get_task_code <- function(task) {
  task_code <- NULL
  task_code <- paste0(task_code, "# include libraries <br>")
  task_code <- paste0(task_code, "library(mlr3) <br>")
  task_code <- paste0(task_code, "library(mlr3learners) <br>")
  task_code <- paste0(task_code, "library(mlr3pipelines) <br>")
  if (task$id == "iris" || task$id == "mtcars" || task$id == "german_credit") {
    task_code <- paste0(task_code, "# using pre-defined mlr3-tasks <br>")
    task_code <- paste0(task_code, "task <- tsk(\"", task$id,"\") <br>")
  } else {
    # checking if quote is set as " or ' and choose the other one in read.csv so its either "'" or '"'
    if (input$Data_train_quote == "'") {
      quote_seperator <- '"'
    } else {
      quote_seperator <- "'"
    }
    task_code <- paste0(task_code, "# creating custom task from input data <br>")
    # building the read.csv using the user input
    task_code <- paste0(task_code, "data <-read.csv(file = \"path_to_file\", header= ",
      input$Data_train_header, ", sep = \"", input$Data_train_sep, "\", quote =", quote_seperator,
      input$Data_train_quote,  quote_seperator, ", stringsAsFactors = TRUE) <br>")
    if (is.numeric(currenttask$target)) {
      task_code <- paste0(task_code,
      "task <- TaskRegr$new(id = \"newData\", backend = data, target = ",
      input$Task_target, ")")
    } else if (is.factor(currenttask$target)) {
      task_code <- paste0(task_code,
      "task <- TaskClassif$new(id = ", input$Task_id, ", backend = data, target = ",
      input$Task_target, ")")
    }
  }
  return(task_code)
}

## old version
# get_learner_code <- function(learner) {
#   # creating initial graph
#   learner_code <- "# create initial graph <br>"
#   learner_code <- paste0(learner_code, "graph <- Graph$new() <br>")
#   # adding graph learner to graph
#   learner_name <- learner$graph$ids()[grep("\\.", learner$graph$ids())]
#   learner_code <- paste0(learner_code, "# adding learner PipeOp <br>")
#   if(!isTRUE(currenttask$task$properties == "twoclass")){
#     learner_code <- paste0(learner_code, "graph$add_pipeop(lrn(\"", learner_name, "\", predict_type = \"", learner$predict_type, "\")) <br>")
#     }
#   if(isTRUE(currenttask$task$properties == "twoclass")){
#     learner_code <- paste0(learner_code, "graph$add_pipeop(lrn(\"", learner_name, "\", predict_type = \"prob\")) <br>")
#   }
#   if (any(grepl("encode", learner$graph$ids()))) {
#     learner_code <- paste0(learner_code,
#     "# adding a PipeOp to enable the usage of factor columns for the chosen learner <br>")
#     learner_code <- paste0(learner_code,
#     "graph <- po(\"encode\", method = \"treatment\", affect_columns = selector_type(\"factor\")) %>>% graph <br>")
#   }
#   if (any(grepl("colapply", learner$graph$ids()))) {
#     learner_code <- paste0(learner_code, "# adding a colapply PipeOp  <br>")
#     learner_code <- paste0(learner_code,
#     "graph <- po(\"colapply\", applicator = as.integer, affect_columns = selector_type(\"ordered\")) %>>% graph <br>")
#   }
#   if(isTRUE(currenttask$task$properties == "twoclass")){
#     learner_code <- paste0(learner_code, "# adding a threshold PipeOp for twoclass task <br>")
#     learner_code <- paste0(learner_code, "graph <- graph %>>% po(\"threshold\") <br>")
#   }
#   for (parameter in names(learner$param_set$values)) {
#     learner_code <- paste0(learner_code, "graph$param_set$values$", parameter, "<- ", learner$param_set$values[parameter], "<br>")
#   }
# 
#   learner_code <- paste0(learner_code, "# saving the graph as a GraphLearner <br>")
#   learner_code <- paste0(learner_code, "learner <- as_learner(graph) <br>")
#   return(learner_code)
# }

get_learner_code <- function(learner) {
  learner_name <- learner$graph$ids()[grep("\\.", learner$graph$ids())]
  # creating initial learner
  learner_code <- "# create initial learner <br>"
  if(!isTRUE(currenttask$task$properties == "twoclass")){
    learner_code <- paste0(learner_code, "learner <- lrn(\"", learner_name, "\") <br>")
  }
  if(isTRUE(currenttask$task$properties == "twoclass")){
    learner_code <- paste0(learner_code, "graph$add_pipeop(lrn(\"", learner_name, "\", predict_type = \"prob\")) <br>")
  }  
  
  # create graph learner
  learner_code <- paste0(learner_code, "# create graph of processing chain <br>")
  if(input[["Task_robustify"]]){
    learner_code <- paste0(learner_code, "graph <- pipeline_robustify(task, learner) %>>% learner <br>")
    learner_code <- paste0(learner_code, "# (Note: Additional hyperparameters for robustification can be specified.) <br>")
    
  }
  if(!input[["Task_robustify"]]){
    learner_code <- paste0(learner_code, "graph <- as_graph(po(\"learner\", learner)) <br>")
  }
  
  if (isTRUE(currenttask$task$properties == "twoclass")){
    learner_code <- paste0(learner_code, "# add threshold parameter for twoclass classification <br>")
    learner_code <- paste0(learner_code, "graph <- graph %>>% po(\"threshold\") <br>")
  } 
  
  learner_code <- paste0(learner_code, "# saving the graph as a GraphLearner <br>")
  learner_code <- paste0(learner_code, "learner <- as_learner(graph) <br>")

  # set parameters
  possibleparams <- c("threshold",
                       "rpart.minsplit","rpart.maxdepth","rpart.cp",
                       "ranger.num.trees", "ranger.mtry", "ranger.min.node.size",
                       "svm.kernel","svm.cost", "svm.gamma", "svm.degree",
                       "xgboost.eta", "xgboost.max_depth", "xgboost.nrounds", "xgboost.colsample_bytree", "xgboost.booster")
  # REM: ugly brute force list of currently implemented learners/parameters. should ideally be created automatically (problem: some nonempty fileds in graph$param_set$value although not specified
  
  pars_set <- 0
  for (parameter in names(learner$param_set$values)) {
      inlist <- sapply(possibleparams, function(z) length(grep(z, parameter)))
      if(any(inlist > 0)) {
        pars_set <- pars_set + 1
        if(pars_set == 1) learner_code <- paste0(learner_code, "<br># set hyperparameters <br>")
        learner_code <- paste0(learner_code, "graph$param_set$values$", parameter, " <-  ", learner$param_set$values[parameter], "<br>")
        }
  }
  return(learner_code)
  
}


get_training_code <- function() {
  train_code <- NULL
  if (!is.null(input$TrainFit_seed)) {
    train_code <- paste0(train_code,
    "# setting the seed for reproduction <br>", "set.seed(", input$TrainFit_seed, ") <br>")
  }
  train_code <- paste0(train_code, "# creating split for test and training data <br>")
  if (!is.null(input$TrainFit_input_split)) {
    train_code <- paste0(train_code, "# using the split set by the user <br>")
    train_code <- paste0(train_code, "train_data <- sample(task$row_ids, task$nrow*",
      input$TrainFit_input_split / 100, ") <br>")
  } else {
    train_code <- paste0(train_code, "# using default 80/20 split <br>")
    train_code <- paste0(train_code, "train_ids <- sample(task$row_ids, task$nrow*0.8) <br>")
  }
  train_code <- paste0(train_code, "test_ids <- setdiff(task$row_ids, train_ids) <br>")
  train_code <- paste0(train_code, "# training the model <br>")
  train_code <- paste0(train_code, "learner$train(task, row_ids = train_ids) <br>")
  train_code <- paste0(train_code, "# predicting on the training and test data <br>")
  train_code <- paste0(train_code, "train_pred <- learner$predict(task, row_ids = train_ids) <br>")
  train_code <- paste0(train_code, "test_pred <- learner$predict(task, row_ids = test_ids) <br>")
  return(train_code)
}

get_score_code <- function(task, learner) {
  score_code <- NULL
  score_code <- paste0(score_code, "# scoring the test prediction with one of the following measures <br>")
  score_code <- paste(score_code,
  "# available measures: ", toString(unname(get_msrs(task, learner, avail_msrs, msr_translations))), "<br>")
  score_code <- paste0(score_code,
  "measure <- msr(\"",unname(get_msrs(task, learner, avail_msrs, msr_translations))[1], "\") <br>")
  score_code <- paste0(score_code, "test_pred$score(measure) <br>")
  return(score_code)
}

get_resampling_code <- function() {
  resampling_code <- "# performing resampling <br>"
  resampling_code <- paste0(resampling_code, "# possible strategies: cv, holdout, bootstrap <br>")
  resampling_code <- paste0(resampling_code, "# example for holdout with a set ratio: <br>")
  resampling_code <- paste0(resampling_code, "resampling <- rsmp(\"holdout\", ratio = 0.5) <br>")
  resampling_code <- paste0(resampling_code, "# more information regarding possible strategies <br>")
  resampling_code <- paste0(resampling_code,
  "# and their parameters can be found here:
  <a href=\"https://mlr3book.mlr-org.com/resampling.html\">mlr3 manual</a><br>")
  resampling_code <- paste0(resampling_code, "rr <- resample(task, learner, resampling)<br>")
  resampling_code <- paste0(resampling_code,
  "# evaluating the performance across all resampling iterations using the already defined measure<br>")
  resampling_code <- paste0(resampling_code, "rr$aggregate(measure)<br>")
  return(resampling_code)
}

get_final_training_code <- function(task, learner) {
  if (input$Predict_data_quote == "'") {
    quote_seperator <- '"'
  } else {
    quote_seperator <- "'"
  }

  final_train_code <- "# training the model on the whole dataset <br>"
  final_train_code <- paste0(final_train_code, "learner$train(task) <br>")
  if (!is.null(input$Data_train_type)) {
  final_train_code <- paste0(final_train_code, "# read new data<br>")
  final_train_code <- paste0(final_train_code, "new_data <-read.csv(file = \"path_to_file\", header= ",
  input$Predict_data_header, ", sep = \"", input$Predict_data_sep, "\", quote =", quote_seperator,
  input$Predict_data_quote, quote_seperator, ", stringsAsFactors = TRUE) <br>")
  final_train_code <- paste0(final_train_code, "# predict on new data <br>")
  final_train_code <- paste0(final_train_code, "learner$predict_newdata(task, newdata = new_data)")
  } else {
  final_train_code <- "# predicting on the whole dataset <br>"
  final_train_code <- paste0(final_train_code, "learner$predict(task) <br>")
  }
  return(final_train_code)
}

observe({
  toggle(id = "Pred_well_decision_tree", 
  condition = !is.null(input$Pred_learner)
    && ((Pred$Learner$graph_model$output$op.id %in% c("classif.rpart", "regr.rpart")) 
    | !is.null(Pred$Learner$graph_model$pipeops$classif.rpart)))
    #last part of condition is needet for twoclass classifcations
})

raise_alert <- function(message, bttn_confirm=FALSE) {
  if (!bttn_confirm) {
    shinyalert(
      title = "Warning",
      text = message,
      animation = FALSE,
      showConfirmButton = TRUE,
      )
  }
  else {
    shinyalert(
      title = "Warning",
      text = message,
      animation = FALSE,
      showCancelButton = TRUE,
      showConfirmButton = TRUE,
      callbackR = function(x) {if (x == TRUE) {render_decision_tree(TRUE)}})
  }
}

render_decision_tree <- function(decision_overwrite=FALSE) {
  node_limit <- 15
  if (!is.null(Pred$Learner$graph_model$pipeops$classif.rpart)) {
    nodes <- nrow(Pred$Learner$graph_model$pipeops$classif.rpart$learner_model$model$frame)
    if (nodes <= node_limit | decision_overwrite) {
      output$plot_decision_tree <- renderPlot(autoplot(Pred$Learner$graph_model$pipeops$classif.rpart$learner_model, type="ggparty"))
    }
    else {
      raise_alert(sprintf("Decision Tree might take a while to render and might not fit the designated space, because there are many (%s) nodes", nodes), TRUE)
    }
    
  }
  else if (Pred$Learner$graph_model$output$op.id == "regr.rpart") {
      nodes <- nrow(Pred$Learner$graph_model$pipeops$regr.rpart$learner_model$model$frame)
      if (nodes <= node_limit | decision_overwrite) {
        output$plot_decision_tree <- renderPlot(autoplot(Pred$Learner$graph_model$pipeops$regr.rpart$learner_model, type="ggparty"))
      }
      else {
        raise_alert(sprintf("Decision Tree might take a while to render and might not fit the designated space, because there are many (%s) nodes", nodes), TRUE)
      }
  }
  else (
    raise_alert("Error: No decision tree")
  )
}

observeEvent(input$action_visualize_predict, {
  if (Pred$Learner_Ov[[4]] == "trained") {
    output$show_viz <- reactive(TRUE)
    outputOptions(output, "show_viz", suspendWhenHidden = FALSE)
    render_decision_tree()
  }
  else {
    raise_alert("Learner must be trained to visualize decision tree")
  }
  
})

output$plotDecisionTree <- renderUI({
  tagList(
    conditionalPanel(condition="output.show_viz == true", plotOutput(outputId = "plot_decision_tree"))
    )
})

observeEvent(input$Pred_learner, {
  output$plot_decision_tree <- renderPlot({})
  output$show_viz <- reactive(FALSE)
    outputOptions(output, "show_viz", suspendWhenHidden = FALSE)
})