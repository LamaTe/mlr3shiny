# reactive values for resampling
Res <- reactiveValues(Graph = NULL, Current_Learner = NULL, Strat = NULL, R_Res = NULL, Perf_Aggr = NULL, Overview = NULL)

## Functions
# get Resampling Workflow data
getResPerfAggr <- function() {
  if (is.null(Res$Perf_Aggr)) {
    return("[not available]")
  }
  else {
    resPerf <- character(0)
    for (i in names(Res$Perf_Aggr)) {
     resPerf <- paste(c(resPerf, paste(i, round(Res$Perf_Aggr[[i]], 3), sep = ": ")), collapse = ", ")
    }
    return(resPerf)
  }
}

getResIters <- function() {
  if (is.null(Res$R_Res)) {
    return("[not available]")
  }
  else {
    return(Res$Strat$iters)
  }
}

createResOverview <- function() {
  overview <- list(
    Task <- currenttask$task$id,
    Learner <- paste(input$Res_learner, Res$Current_Learner$id, sep = " "),
    Strategy <- input$Res_strategy,
    Iterations <- getResIters(),
    "Aggregated Performance" <- getResPerfAggr()
  )
  return(overview)
}

getResOverviewUi <- function() {
  if (!is.null(Res$Current_Learner)) {
    overviewUi <- tagList(
      hr(style = "border-color: #3e3f3a;"),
      fluidRow(
        column(12,
               h5("Resampling Overview", style = "font-weight: bold;"),
               addOverviewLineWf("Task: ", Res$Overview[[1]]),
               addOverviewLineWf("Learner: ", Res$Overview[[2]]),
               addOverviewLineWf("Resampling Strategy: ", Res$Overview[[3]]),
               addOverviewLineWf("Performed Iterations: ", Res$Overview[[4]]),
               addOverviewLineWf("Aggregated Performance: ", Res$Overview[[5]])
        )
      )
    )
    return(overviewUi)
  }
}

# get resampling parameter ui
stratify <- function(id, default) {
  stratifyui <- tagList(
    column(12,
           checkboxInput(inputId = paste0(id, "_stratify"), label = "Stratification", value = default)
           )
  )
  return(stratifyui)
}

ratio <- function(id, default) {
  ratioui <- tagList(
    column(6,
           numericInput(inputId = paste0(id, "_ratio"), label = h5("Fraction in (0, 1) of the data used for training the model"), value = default, min = 0, max = 1)
           )
  )
  return(ratioui)
}

folds <- function(id, default) {
  foldsui <- tagList(
    column(6,
           numericInput(inputId = paste0(id, "_folds"), label = h5("Iterations"), value = default, min = 1)
    )
  )
  return(foldsui)
}

repeats <- function(id, default) {
  repeatsui <- tagList(
    column(6,
           numericInput(inputId = paste0(id, "_repeats"), label = h5("Repetitions"), value = default, min = 1)
           )
  )
  return(repeatsui)
}

# get available parameter per learner for resampling
getResParams <- function() {
  if (is.null(Res$Current_Learner)) {
    return(column(12,
                  h5("No learner has been selected yet.")
                  )
    )
  }
  else {
    params <- tagList()
    for (i in rev(Res$Strat$param_set$ids())) {
        params <- tagAppendChild(params, get(i)(id = "Res", default = Res$Strat$param_set$values[[i]]))
    }
    return(params)
  }
}

# get Learner and resampling choices
getLearnResChoicesUI <- function() {
  if (is.null(LearnerMeta$Learner_Avail)) {
    ui <- tagList(
      column(12,
             h5("No learner has been created yet.")
      )
    )
    return(ui)
  }
  else {
    ui <- tagList(
      column(6,
             radioButtons(inputId = "Res_learner", label = h5("Select a new learner:"), choices = LearnerMeta$Learner_Avail,
                          selected = character(0))
      ),
      column(6,
             radioButtons(inputId = "Res_strategy", label = h5("Select a resampling strategy:"),
                          choices = c("holdout", "cross-validation" = "cv", "bootstrap"))
      )
    )
    return(ui)
  }
}

# get available measures
getResMeasuresUi <- function() {
  if (!is.null(Res$R_Res)) {
  measureui <- tagList(
    hr(style = "border-color: #3e3f3a;"),
    h5("Measure Aggregated Performance", style = "font-weight: bold;"),
    fluidRow(
      column(6,
             selectizeInput(inputId = "Res_measures", label = NULL,
                            choices = get_msrs(currenttask$task, Res$Current_Learner, avail_msrs, msr_translations),
                            options = list(
                              placeholder = 'Nothing selected',
                              onInitialize = I('function() { this.setValue(""); }')
                            ),
                            multiple = TRUE)
      ),
      column(6,
             actionButton(inputId = "Res_aggr_measure", label = "Score", style = "float: right;")
      )
    )
  )
  return(measureui)
  }
}

getResButton <- function() {
  if (!is.null(Res$Current_Learner)) {
    buttonui <- tagList(
      hr(style = "border-color: #3e3f3a;"),
      fluidRow(
        column(12,
               div(style = "display:inline-block; width:100%; text-align: center;",
                   actionButton(inputId = "Res_resample", label = "Perform Resampling"),
                   h5("Resampling large datasets may take a while.")
               )
        )
      )
    )
    return(buttonui)
  }
}

getResTable <- function() {
  if (!is.null(Res$R_Res)) {
    ui <- DT::datatable(as.data.table(Res$R_Res$prediction()),
                options = list(scrollX = TRUE,searching = FALSE, bInfo = FALSE, lengthChange = FALSE))
  }
  else {
    ui <- NULL
  }
  return(ui)
}

getResDownload <- function() {
  if (!is.null(Res$R_Res)) {
    downloadbtnui <- tagList(
      hr(style = "border-color: #3e3f3a;"),
        div(style = "display:inline-block; width:100%; text-align: center;",
            #hidden(
              downloadButton(outputId  = "Res_result_download", label = "Export Resampling Result")
             # )
        )
    )
  }
}

## outputs and observers
# make Learner and resampling choices ui
output$Res_learner_selection <- renderUI({
    getLearnResChoicesUI()
})

output$Res_overview <- renderUI({
  getResOverviewUi()
})

# set resampling strategy
observeEvent(input$Res_strategy, {
  Res$R_Res <- NULL
  Res$Perf_Aggr <- NULL
  Res$Strat <- rsmp(input$Res_strategy)
  Res$Overview <- createResOverview()
})

# make params ui
output$Res_params <- renderUI({
    getResParams()
})

# make measure and resample button ui
output$Res_measure <- renderUI({
    getResMeasuresUi()
})

output$Res_resample_button <- renderUI({
    getResButton()
})

# set hyperparams for strategy and perform resampling
observeEvent(input$Res_resample, {
  paramsres <- list()
  for (i in Res$Strat$param_set$ids()) {
    paramsres[[i]] <- input[[paste0("Res_", i)]]
  }

  withProgress(message = "Performing resampling strategy",
    withCallingHandlers(
      tryCatch({Res$Strat$param_set$values <- paramsres
      set.seed(42)
      incProgress(0.2)
      Res$Graph <- Graph$new()
      Res$Graph$add_pipeop(Res$Current_Learner)
      Res$Current_Learner <- as_learner(Res$Graph)
      Res$R_Res <- resample(task = currenttask$task, learner = Res$Current_Learner, resampling = Res$Strat)
      incProgress(0.5)
      },
      error = errorAlertResample
      ),
      warning = warningAlert
    )
  )
  Res$Overview <- createResOverview()
})

observeEvent(input$Res_aggr_measure, {
  withCallingHandlers(
    tryCatch(Res$Perf_Aggr <- Res$R_Res$aggregate(msrs(c(input$Res_measures))),
             error = errorAlertResample),
    warning = warningAlert
  )
  Res$Overview <- createResOverview()
})



output$Res_pred_view <- DT::renderDataTable({
  getResTable()
})
observe({
  toggle(id = "Res_well_prediction", condition = !is.null(Res$R_Res))
})

output$Res_download <- renderUI({
  getResDownload()
})

output$Res_result_download <- downloadHandler(
  filename = function() {
    paste(paste("ResamplingResult", Res$Strat$id, Res$Current_Learner$id, sep = "_"), ".rds", sep = "")
  },
  content = function(file) {
    saveRDS(Res$R_Res, file = file)
  }
)

# start or reset resampling workflow
# set current learner
resetRes <- function() {
  Res$Current_Learner = NULL
  Res$R_Res = NULL
  Res$Perf_Aggr = NULL
  Res$Overview = NULL
  #Res$Strat = NULL
}

observeEvent(input$Res_learner, {
  resetRes()
  Res$Current_Learner <- get(input$Res_learner)$Learner$clone(deep = TRUE)
  Res$Overview <- createResOverview()
})

observeEvent(currenttask$task, {
  resetRes()
})

