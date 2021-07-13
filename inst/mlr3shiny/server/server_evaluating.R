# reactive values for the chosen learner, the features selected and the iml objects used for explaining the model
eval_meta <- reactiveValues(current_learner = NULL, selected_features = NULL, feature_effect = NULL, feature_importance = NULL, feature_importance_compare = NULL)

# available losses for multiclass/twoclass classification and regression tasks
classif_losses <- c("ce", "f1")
regr_losses <- c(
  "mae", "mse", "rsme", "mape",
  "mdae", "msle", "percent_bias", "rae", "rmsle", "rse", "rrse", "smape"
)
twoclass_losses <- c("logLoss")

# displaying the learner selection in the ui
output$eval_learner_selection <- renderUI({
  get_learner_selection()
})

# if a learner is selected display a list of possible features to select
output$eval_learner_feat_selection <- renderUI({
  get_feature_selection()
})

output$eval_loss_function_selection <- renderUI({
  get_loss_function_list()
})

output$eval_learner_plot_tabs <- renderUI({
  display_plot_tabs()
})

output$eval_compare_method_selection <- renderUI({
  get_compare_method_list()
})

# observe "start evaluation" button and start iml workflow
observeEvent(input$evaluate_start, {
  model <- Predictor$new(eval_meta$current_learner, data = currenttask$task$data(), y = currenttask$task$target_names)
  # saving iml calculations in meta object
  eval_meta$feature_importance <- FeatureImp$new(model, loss = input$loss_picker)
  eval_meta$feature_effect <- FeatureEffects$new(model)
  calculate_plots()
})

# observe choosen learner in ui and change meta object
observeEvent(input$selected_learner, {
  reset_evaluation()
  eval_meta$current_learner <- get(input$selected_learner)$Learner$clone(deep = TRUE)
  eval_meta$current_learner$train(task = currenttask$task)
  # showing only relevant loss functions
  reset_plots()
})

# save selected features and reset plot diagrams
observeEvent(input$feat_picker, {
  reset_plots()
  eval_meta$selected_features <- input$feat_picker
})

# builder for compare method selection
get_compare_method_list <- function() {
  ui <- wellPanel(
    tagList(
      fluidRow(
        column(
          12,
          h5("Select compare method for Feature Importance:")
        ),
        column(
          12,
          pickerInput("compare_picker",
          choices = c("ratio", "difference"))
        )
      )
    )
  )
  return(ui)
}


# get relevant loss functions for current task
get_loss_function_list <- function() {
  if (!is.null(input$selected_learner)) {
    if (currenttask$task$task_type == "classif") {
      if (currenttask$task$properties == "twoclass") {
        ui <- loss_ui_builder(twoclass_losses)
      } else {
        ui <- loss_ui_builder(classif_losses)
      }
    } else if (currenttask$task$task_type == "regr") {
      ui <- loss_ui_builder(regr_losses)
    }
    return(ui)
  }
}

# builder for loss function selection
loss_ui_builder <- function(choices) {
  wellPanel(
    tagList(
      fluidRow(
        column(
          12,
          h5("Select loss function for Feature Importance: "),
        ),
        column(
          12,
          h5("Current Task Type:", currenttask$task$task_type)
        )
      ),
      fluidRow(
        column(
          12,
          pickerInput("loss_picker",
            choices = choices
          )
        )
      )
    )
  )
}

# get all possible features from the selected learner
get_feature_selection <- function() {
  if (!is.null(input$selected_learner)) {
    ui <- wellPanel(
      tagList(
        fluidRow(
          column(
            12,
            h5("Select Features to show in PD-Plot: (max. 6)")
          )
        ),
        fluidRow(
          column(
            12,
            pickerInput("feat_picker",
              choices = c(currenttask$featNames),
              multiple = TRUE,
              options = pickerOptions(
                maxOptions = 6
              )
            )
          )
        ),
      )
    )
  }
}

# of all existing learners return a list of the already trained ones
# this loop is necessary as it's not possible to loop through a reactiveValue like "trained_learner_list"
get_trained_learners <- function() {
  list_of_learners <- list()
  for (learner in LearnerMeta$Learner_Avail) {
    if (!is.null(trained_learner_list[[learner]])) {
      list_of_learners <- append(list_of_learners, learner)
    }
  }
  return(list_of_learners)
}


# get all possible learners
get_learner_selection <- function(list_of_learners) {
  if (length(reactiveValuesToList(trained_learner_list)) == 0) {
    ui <- tagList(
      fluidRow(
        column(
          12,
          h5("No learner has been trained on the whole dataset, go back to the previous tab to train on the whole dataset")
        )
      )
    )
    return(ui)
  }
  else {
    ui <- tagList(
      fluidRow(
        column(
          12,
          radioButtons(
            inputId = "selected_learner",
            label = h5("Select a learner to evaluate its performance."),
            choices = get_trained_learners(),
            selected = character(0)
          )
        ),
        HTML("<br/>"),
        fluidRow(
          column(
            12,
            div(
              style = "display:inline-block; width:100%; text-align: center;",
              actionButton(inputId = "evaluate_start", label = "Start evaluating")
            )
          )
        )
      )
    )
    return(ui)
  }
}

calculate_plots <- function() {
  output$pdp_plot <- renderPlot({
    plot(eval_meta$feature_effect, features = eval_meta$selected_features)
  })
  output$vi_plot <- renderPlot({
    plot(eval_meta$feature_importance)
  })
}

# display a tabPanel for the PDP and VI plots
display_plot_tabs <- function() {
  if (!is.null(eval_meta$feature_importance) && !is.null(eval_meta$feature_effect) && !is.null(input$selected_learner)) {
    ui <- tabsetPanel(
      type = "tabs",
      tabPanel(
        "Feature Importance",
        wellPanel(
          plotOutput(outputId = "vi_plot")
        )
      ),
      tabPanel(
        "PD-Plot",
        wellPanel(
          plotOutput(outputId = "pdp_plot")
        )
      )
    )
    return(ui)
  }
}



# helper for resetting the eval_meta object
reset_evaluation <- function() {
  eval_meta$current_learner <- NULL
  eval_meta$feature_effect <- NULL
  eval_meta$feature_importance <- NULL
}

# helper for resetting the plots in the ui
reset_plots <- function() {
  output$pdp_plot <- NULL
  output$vi_plot <- NULL
}