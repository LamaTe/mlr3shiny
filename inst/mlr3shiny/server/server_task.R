# reactive values for task
currenttask <- reactiveValues(task = NULL, overview = NULL, target = NULL, featNames = NULL, featTypes = NULL, positive = NULL, tableOptions = NULL)
# in case a feature gets dropped make sure to only include the same features when predicting new data
features_to_use <- reactiveValues(features = NULL)

# render sidebarPanel depending on input for Task
observe({
  if (input$Task_backend == "iris" || input$Task_backend == "mtcars" || input$Task_backend == "german_credit") {
    currenttask$task <- mlr_tasks$get(input$Task_backend)
  }
  else if (is.null(data$traindata) && input$Task_backend == "imported training data" ) {
    shinyalert(title = "Task Creation", text = userhelp[["Task Creation"]], closeOnClickOutside = TRUE, animation = FALSE)
  }
  else if (!is.null(data$traindata) && input$Task_backend == "imported training data") {
    output$Task_make_id <- renderUI({
      textInput(inputId = "Task_id", label = h5("Task ID"), value = "my_task")
    })
    choices <- colnames(data$traindata)
    output$Task_make_target <- renderUI({
      selectInput(inputId = "Task_target", label = h5("Task Target"), choices = choices,
                  selected = choices[length(choices)])
    })
    output$Task_make_task <- renderUI({
      div(style = "display:inline-block; width:100%; text-align: center;",
          actionButton(inputId = "Task_make", label = "Create Task", icon = icon("bookmark"))
      )
    })
  }
})

observeEvent(currenttask$task,{
  reset_trained_learner_list()
})

observe({
  toggle(id = "Task_target", condition = (input$Task_backend == "imported training data"))
  toggle(id = "Task_id", condition = (input$Task_backend == "imported training data"))
  toggle(id = "Task_make", condition = (input$Task_backend == "imported training data"))
})

# decide whether it is a classification or regression task
observeEvent(input$Task_make, {
  currenttask$target <- data$traindata[, input$Task_target]

  if (is.numeric(currenttask$target)) {
    currenttask$task <- TaskRegr$new(id = input$Task_id, backend = data$traindata, target = input$Task_target)
  }
  else if (is.factor(currenttask$target)) {
    currenttask$task <- TaskClassif$new(id = input$Task_id, backend = data$traindata, target = input$Task_target)
  }
  else {
    shinyalert(title = "Target Selection",
               text = userhelp[["Task Creation Target"]], closeOnClickOutside = TRUE, animation = FALSE)
  }
})

# Task Summary
observe({
  # get bad features
  # To-DO: What to do when prediction data have unsupported features? -> for preprocessing later on
  allfeat <- currenttask$task$feature_types
  bad <- c("POSIXct", "complex", "Date")
  badfeat <- allfeat[which(allfeat[, 2]$type %in% bad), ]$id
  features_to_use$features <- allfeat[!badfeat,]$id
  # deactivate unwanted features
  currenttask$task$select(cols = features_to_use$features)

  if (length(badfeat)) {
    shinyalert(title = "Features Dropped", text = userhelp[["Features Dropped"]], closeOnClickOutside = TRUE, animation = FALSE)
  }

  ### mlr task is R6 Object, Shiny cannot see, when this object's state changes cause its modified in place
  ### to ensure that the table still updates when the features are removed later on, assign it an extra reactive value
  currenttask$featTypes <- currenttask$task$feature_types
  currenttask$featNames <- currenttask$task$feature_names
  if (!identical(currenttask$task$properties, character(0)) && currenttask$task$properties == "twoclass") {
    currenttask$positive <- currenttask$task$positive
  }
  # add positive label if twoclass
  currenttask$overview <- list(
    task_id <- currenttask$task$id,
    task_property = currenttask$task$properties,
    task_type = currenttask$task$task_type,
    cols = currenttask$task$ncol,
    observations = currenttask$task$nrow,
    target = c(currenttask$task$target_names),
    features = currenttask$featTypes
  )
})

# datatable options
observe({
  if (nrow(currenttask$featTypes) > 4) {
    currenttask$tableOptions <- list(paging = FALSE, searching = FALSE,
                   bInfo = FALSE, ordering = FALSE, width = "250px",
                   scrollY = "130px")
  }
  else {
    currenttask$tableOptions <- list(paging = FALSE, searching = FALSE,
                                     bInfo = FALSE, ordering = FALSE, width = "250px")
  }
})

#### angepasst aus shinymlr
addOverviewLineTask = function(title, body) {
  fluidRow(
    column(4, h5(title)),
    column(8, h5(body))
  )
}

printTaskOverviewUI = function() {
  tagList(
            h5("Task Overview", style = " font-weight: bold;"),
            addOverviewLineTask("Supervised Task: ", paste(currenttask$overview[[2]], currenttask$overview[[3]], sep = " ")),
            addOverviewLineTask("Task ID: ", currenttask$overview[[1]]),
            addOverviewLineTask("Data: ", paste(currenttask$overview[[4]], "Variables with",
                                            currenttask$overview[[5]], "Observations", sep = " ")),
            addOverviewLineTask("Target: ", currenttask$overview[[6]]),
            if (!identical(currenttask$task$properties, character(0)) && currenttask$task$properties == "twoclass") {
            addOverviewLineTask("Positive Class: ", currenttask$positive)
              },
            addOverviewLineTask("Features: ", renderDataTable(expr = as.data.table(currenttask$overview[[7]]), rownames = FALSE,
                                                          options = currenttask$tableOptions)
                            )
  )
}
###

output$Task_overview <- renderPrint({
  printTaskOverviewUI()
})



# Task processing

observeEvent(input$Task_feat_deactivate, {
    #updatedfeat <- setdiff(currenttask$task$feature_names, input$Task_feature)
    #currenttask$task$select(cols = updatedfeat)
    currenttask$task$select(cols = input$Task_feature)
  
    ## here we need to update currenttask$features, so that Shiny recognizes that the R6- task - object has changed
    currenttask$featTypes <- currenttask$task$feature_types
    currenttask$featNames <- currenttask$task$feature_names
})

observeEvent(input$Task_change_pos_class, {
  currenttask$task$positive <- input$Positive_class
  ## here we need to update currenttask$positive, so that Shiny recognizes that the R6- task - object has changed
  currenttask$positive <- currenttask$task$positive
})

printTaskProcessingUI <- function(){
  tagList(
    h5("Task Processing", style = "font-weight: bold;"),
    if (!identical(currenttask$task$properties, character(0)) && currenttask$task$properties == "twoclass") {
      fluidRow(column(4,
                      h5('Set Positive Class:')),
               column(4,selectizeInput(inputId = "Positive_class", label = NULL,
                                       choices = c(currenttask$task$class_names),
                                       options = list(
                                         placeholder = 'Nothing selected',
                                         onInitialize = I('function() { this.setValue(""); }')
                                       ),
                                       multiple = FALSE)),
               column(4, actionButton(inputId = "Task_change_pos_class", label = "Change", style = "float: right;")))
    },
    fluidRow(
      column(4, h5("Select Features: ")),
      column(4,  pickerInput("Task_feature",
                             choices = c(currenttask$featNames),
                             multiple = TRUE,
                             selected = c(currenttask$featNames),
                             options = pickerOptions(
                               #  list(
                               #   placeholder = 'Nothing selected',
                               #   onInitialize = I('function() { this.setValue(""); }')
                               # )
                               list(`actions-box` = TRUE)
                               )
                             )
             ),
      #now select instead of dtop / deactivate
      column(4, actionButton(inputId = "Task_feat_deactivate", label = "Select", style = "float: right;"))
    )
  )
}


output$Task_processing <- renderUI({
  printTaskProcessingUI()
})



