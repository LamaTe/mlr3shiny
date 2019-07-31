Bench <- reactiveValues(Bench_Rslt = NULL, Res_Strat = NULL, Current_Learners = NULL, Current_Design = NULL, Best_Learner = NULL,
                        Overview = NULL)

## Functions
# get Learner and resampling choices
getLrnsStrtsUI <- function() {
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
        column(6,
               checkboxGroupInput(inputId = "Bench_learners", label = h5("Select learners to include in benchmarking:"),
                                  choices = LearnerMeta$Learner_Avail,
                                  selected = character(0))
        ),
        column(6,
               radioButtons(inputId = "Bench_res_strategy", label = h5("Select a resampling strategy:"),
                            choices = c("holdout", "cross-validation" = "cv", "bootstrap"))
        )
      ),
      HTML("<br/>"),
      fluidRow(
        column(12,
               div(style = "display:inline-block; width:100%; text-align: center;",
                   actionButton(inputId = "Bench_start", label = "start benchmarking")
               )
               )
      )
      )
    return(ui)
  }
}

# get learnerobjects of selected learners and assign them to Bench$Current_Learners
setLrnsObjects <- function() {
  for (i in input$Bench_learners) {
    Bench$Current_Learners[[i]] <- get(i)$Learner$clone(deep = TRUE)
  }
}


# create benchmark overview
getItersOv <- function() {
  if (is.null(Bench$Bench_Rslt)) {
    return("[not available]")
  }
  else {
    return(paste(nrow(Bench$Bench_Rslt$data), "iterations in", uniqueN(Bench$Bench_Rslt$data$hash),"resamplings", sep = " "))
  }
}

getCurrentLearnersOv <- function() {
  if (!is.null(Bench$Current_Learners)) {
    lrns <- paste(sapply(input$Bench_learners, function(x){
      c(paste(x, Bench$Current_Learners[[x]]$id, sep = ": "))
      }), collapse = ", ")
    return(lrns)
  }
}

getBestLrnOv <- function(){
  if (is.null(Bench$Best_Learner)) {
    return("[not available]")
  }
  else{
    learner <- NULL
    # Fix-Me: pretty ugly solution, replace for with apply function and paste functions with better function for string handling
    for (i in names(Bench$Current_Learners)) {
      if (Bench$Best_Learner$learners[[1L]]$hash == Bench$Current_Learners[[i]]$hash) learner = i
    }
    best <- paste(learner,
                  paste(input$Bench_measure[1], round(Bench$Best_Learner$aggregate(measures = input$Bench_measure[1]), 3), sep = ": "),
                  sep = "; ")
    return(best)
  }
}

createBenchOverview <- function() {
  overview <- list(
    Task <- currenttask$task$id,
    Learners <- getCurrentLearnersOv(),
    Strategy <- input$Bench_res_strategy,
    Iterations <- getItersOv(),
    "Best Learner" <- getBestLrnOv()
  )
  return(overview)
}

getBenchOverviewUi <- function() {
  if (is.null(Bench$Current_Learners)) {
    ui <- tagList(
      hr(style = "border-color: #3e3f3a;"),
      fluidRow(
        column(12,
               h5("Benchmarking has not been started yet.")
        )
      )
    )
    return(ui)
  }
  else {
    overviewUi <- tagList(
      hr(style = "border-color: #3e3f3a;"),
      fluidRow(
        column(12,
               h5("Benchmarking Overview", style = "font-weight: bold;"),
               addOverviewLineWf("Task: ", Bench$Overview[[1]]),
               addOverviewLineWf("Learners: ", Bench$Overview[[2]]),
               addOverviewLineWf("Resampling Strategy: ", Bench$Overview[[3]]),
               addOverviewLineWf("Performed Iterations: ", Bench$Overview[[4]]),
               addOverviewLineWf("Best Learner: ", Bench$Overview[[5]])
        )
      )
    )
    return(overviewUi)
  }
}

# resampling params for benchmarking (param functions are in server_resample)
getBenchParams <- function() {
  if (is.null(Bench$Current_Learners)) {
    return(column(12,
                  h5("Benchmarking has not been started yet.")
                  )
    )
  }
  else {
    params <- tagList()
    for (i in rev(Bench$Res_Strat$param_set$ids())) {
      params <- tagAppendChild(params, get(i)(id = "Bench", default = Bench$Res_Strat$param_set$values[[i]]))
    }
    return(params)
  }
}

# get available measures (not nicest solution since is.null check only necessary to toggle measure ui)
getBenchMeasuresUi <- function() {
  if (!is.null(Bench$Bench_Rslt)) {
    measureui <- tagList(
      hr(style = "border-color: #3e3f3a;"),
      h5("Measure Aggregated Performance", style = "font-weight: bold;"),
      fluidRow(
        column(6,
               selectizeInput(inputId = "Bench_measure", label = NULL,
                              choices = possiblemeasures[[currenttask$task$task_type]],
                              options = list(
                                placeholder = 'Nothing selected',
                                onInitialize = I('function() { this.setValue(""); }')
                              ),
                              multiple = TRUE)
        ),
        column(6,
               actionButton(inputId = "Bench_aggr_measure", label = "Score", style = "float: right;")
        )
      )
    )
    return(measureui)
  }
}

getBenchButton <- function() {
  if (!is.null(Bench$Current_Learners)) {
    buttonui <- tagList(
      hr(style = "border-color: #3e3f3a;"),
      fluidRow(
        column(12,
               div(style = "display:inline-block; width:100%; text-align: center;",
                   actionButton(inputId = "Bench_benchmark", label = "Benchmark"),
                   h5("Benchmarking large datasets may take a while.")
               )
        )
      )
    )
    return(buttonui)
  }
}

getBenchTable <- function() {
  if (!is.null(Bench$Bench_Rslt)) {
    tabl <-  DT::datatable(as.data.table(Bench$Bench_Rslt$aggregate(measures = input$Bench_measure)[,-(1:2)]),
                           options = list(scrollX = TRUE,searching = FALSE, ordering = FALSE, bInfo = FALSE,
                                          lengthChange = FALSE, paging = FALSE))
    return(tabl)
  }
}

getBenchDownload <- function() {
  if (!is.null(Bench$Bench_Rslt)) {
    downloadbtnui <- tagList(
      hr(style = "border-color: #3e3f3a;"),
      div(style = "display:inline-block; width:100%; text-align: center;",
          downloadButton(outputId  = "Bench_result_download", label = "Export Benchmark Result")
      )
    )
  }
}

## Observers
# make Learner and resampling choices ui
output$Bench_learner_strats_selection <- renderUI({
  getLrnsStrtsUI()
})

observeEvent(input$Bench_start, {
  updateActionButton(session = session, inputId = "Bench_start", label = "Restart benchmarking")
})

output$Bench_overview <- renderUI({
  getBenchOverviewUi()
})

output$Bench_params <- renderUI({
  getBenchParams()
})

output$Bench_measures <- renderUI({
  getBenchMeasuresUi()
})

output$Bench_benchmark_button <- renderUI({
  getBenchButton()
})

output$Bench_download <- renderUI({
  getBenchDownload()
})

output$Bench_result_download <- downloadHandler(
  filename = function() {
    paste(paste("BenchmarkResult", Bench$Res_Strat$id, sep = "_"), ".rds", sep = "")
  },
  content = function(file) {
    saveRDS(Bench$Bench_Rslt, file = file)
  }
)

# benchmark
observeEvent(input$Bench_benchmark, {
  paramsbench <- list()
  for (i in Bench$Res_Strat$param_set$ids()) {
    paramsbench[[i]] <- input[[paste0("Bench_", i)]]
  }
  withProgress(message = "Computing benchmark", style = "notification",
               withCallingHandlers(
                 tryCatch({Bench$Res_Strat$param_set$values <- paramsbench
                 set.seed(42)
                 incProgress(0.2)
                 Bench$Current_Design <- expand_grid(tasks = currenttask$task, learners = Bench$Current_Learners, resamplings = Bench$Res_Strat)
                 incProgress(0.4)
                 Bench$Bench_Rslt <- benchmark(Bench$Current_Design)
                 incProgress(0.6)
                 },
                 error = errorAlertBench
                 ),
                 warning = warningAlert
               )
  )
  Bench$Overview <- createBenchOverview()
})

observeEvent(input$Bench_aggr_measure, {
  Bench$Best_Learner <- Bench$Bench_Rslt$best(measure = input$Bench_measure[1])
  output$Bench_rslt_view <- DT::renderDataTable({
    getBenchTable()
  })
  Bench$Overview <- createBenchOverview()
})


observe({
  toggle(id = "Bench_well_rslt", condition = !is.null(Bench$Best_Learner))
})

resetBench <- function() {
  Bench$Current_Learners = NULL
  Bench$Bench_Rslt = NULL
  Bench$Current_Design = NULL
  Bench$Best_Learner = NULL
  Bench$Overview = NULL
}

observeEvent(input$Bench_start, {
  if (!is.null(input$Bench_learners)) {
    resetBench()
    setLrnsObjects()
    Bench$Res_Strat <- mlr_resamplings$get(input$Bench_res_strategy)
    Bench$Overview <- createBenchOverview()
  }
  else {
    shinyalert(title = "No Learner Selected",
               text = paste("In order to start benchmarking learners must be selected.",
                            sep = " "),
               animation = FALSE, closeOnClickOutside = TRUE)
  }
})

observeEvent(currenttask$task, {
  resetBench()
})
