# reactive values for task
currenttask <- reactiveValues(task = NULL, overview = NULL, target = NULL, featNames = NULL, featTypes = NULL, positive = NULL, tableOptions = NULL)
# in case a feature gets dropped make sure to only include the same features when predicting new data
features_to_use <- reactiveValues(features = NULL)
# used for visualization puropses
originalTask <- NULL

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
  output$plot_visualization <- renderPlot({})
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

observeEvent(input$Task_robustify, {
  if(!input[["Task_robustify"]]){
    shinyalert(title = "Notification",
               text = "Robustify data preprocessing disabled. 
                       Note that this might lead to an error 
                       when training a learner if the chosen
                       learner can not deal with all variable types 
                       in the original data.",
               animation = FALSE, 
               closeOnClickOutside = TRUE,
               className="alert-info",)
  }
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
    ),
    fluidRow(
      column(4, h5("Robustify: ")),
      column(8, checkboxInput(inputId = "Task_robustify", label = "apply mlr3 robustify preprocessing to data", value = TRUE),
                conditionalPanel(condition = "input[[\"Task_robustify\"]] == true",
                                 checkboxInput("robustify_details", label = "Show detailed options for robustification", value = FALSE)),
                conditionalPanel(condition = "input[[\"robustify_details\"]] == true",
                                 selectInput("impute_missings", "impute_missings", list("NULL" = "NULL", "TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "NULL")),
                conditionalPanel(condition = "input[[\"robustify_details\"]] == true",
                                 selectInput("factors_to_numeric", "factors_to_numeric", list("NULL" = "NULL", "TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "NULL")),
                conditionalPanel(condition = "input[[\"robustify_details\"]] == true",
                                 numericInput("max_cardinality", label = "max_cardinality", value = 1000, min = 2)),
                conditionalPanel(condition = "input[[\"robustify_details\"]] == true",
                                 selectInput("ordered_action", "ordered_action", 
                                             list("factor" = "factor", "factor!" = "factor!", "matrix" = "matrix", "matrix!" = "matrix!",
                                                  "ignore" = "ignore", "ignore!" = "ignore!"), selected = "factor")),
                conditionalPanel(condition = "input[[\"robustify_details\"]] == true",
                                 selectInput("character_action", "character_action", 
                                             list("factor" = "factor", "factor!" = "factor!", "matrix" = "matrix", "matrix!" = "matrix!",
                                                  "ignore" = "ignore", "ignore!" = "ignore!"), selected = "factor")),
                conditionalPanel(condition = "input[[\"robustify_details\"]] == true",
                                 selectInput("POSIXct_action", "POSIXct_action", 
                                             list("numeric" = "numeric", "numeric!" = "numeric!", "datefeatures" = "datefeatures", "datefeatures!" = "datefeatures!",
                                                  "ignore" = "ignore", "ignore!" = "ignore!"), selected = "factor"))
             
             )
    )
    
  )

}


output$Task_processing <- renderUI({
  printTaskProcessingUI()
})

render_visualization_plot <- function(task) {
  output$plot_visualization <- renderPlot({
    autoplot(task, input$Plot_Type)
    })
  }

observeEvent(input$action_visualize, {
  temp_task <- originalTask$clone(deep = TRUE)

  if (is.null(input$Features_Viz)) {
    #exit code execution if no feature is selected and reset selected to default
    updatePickerInput(session = session, inputId="Features_Viz", selected=originalTask$feature_names)
    shinyalert(
      title = "Warning",
      text = "You need to select Features for Visualization.",
      animation = FALSE,
      showConfirmButton = TRUE,
      className="alert-warning",
    )
    return()
  }
  task <- temp_task$select(cols = input$Features_Viz)

  output$show_viz <- reactive(TRUE)
  outputOptions(output, "show_viz", suspendWhenHidden = FALSE)

  if (task$nrow > 5000 | length(task$feature_names) > 5) {
    shinyalert(
      title = "Information",
      text = "Computation time for the plot might take long, 
      because there are many observations or variables in the Data Backend.
      This might also affect readability of the plot.",
      animation = FALSE,
      showCancelButton = TRUE,
      showConfirmButton = TRUE,
      className="alert-info",
      callbackR = function(x) {if (x == TRUE) {render_visualization_plot(task)}})
  }
  else {
    render_visualization_plot(task)
    }
})

printTaskVisualizeUI <- function(){
  # make a copy of the task so the vizualisation is independent
  originalTask <<- currenttask$task$clone(deep = TRUE)
  plot_features = originalTask$feature_names
  plot_choices = c("target", "duo", "pairs")
  if (originalTask$task_type == "regr") {
    plot_choices = plot_choices[plot_choices != "duo"]
  }
  tagList(
    h5("Visualization of Data", style = "font-weight: bold;"),
    fluidRow(
      column(4, h5("Select Plot Type:")),
      column(4, pickerInput("Plot_Type",
                             choices = plot_choices,
                             selected = "pairs")),
      column(4, actionButton(inputId = "action_visualize", label = "Create Visualization", icon = icon("hammer")))),
      conditionalPanel(condition = "input[[\"Plot_Type\"]] != 'target'", 
        fluidRow(
        column(4, h5("Select Features for Visulization:")),
        column(4, pickerInput("Features_Viz",
                                choices = originalTask$feature_names,
                                multiple = TRUE,
                                selected = originalTask$feature_names)))),
    conditionalPanel(condition="input.action_visualize != 0 && output.show_viz == true", plotOutput(outputId = "plot_visualization"))
  )
}

observeEvent(input$Plot_Type, {
  output$show_viz <- reactive(FALSE)
  outputOptions(output, "show_viz", suspendWhenHidden = FALSE)
  output$plot_visualization <- renderPlot({})
})

output$Task_visualize <- renderUI({
  printTaskVisualizeUI()
})
