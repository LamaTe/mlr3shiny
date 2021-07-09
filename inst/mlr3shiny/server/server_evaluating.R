# reactive values for the chosen learner, the features selected and the iml objects used for explaining the model
eval_meta <- reactiveValues(current_learner = NULL, selected_features = NULL, feature_effect = NULL, feature_importance = NULL)

# displaying the learner selection in the ui
output$eval_learner_selection <- renderUI({
  get_learner_list()
})

# if a learner is selected display a list of possible features to select
output$eval_learner_feat_selection <- renderUI({
  get_feature_list()
})

# observe "start evaluation" button and start iml workflow
observeEvent(input$evaluate_start, {
  model <- Predictor$new(eval_meta$current_learner, data = currenttask$task$data(), y = currenttask$task$target_names)
  # saving iml calculations in meta object
  eval_meta$feature_importance <- FeatureImp$new(model, loss = "ce")
  eval_meta$feature_effect <- FeatureEffects$new(model)
  display_plot_tabs()
})

# observe choosen learner in ui and change meta object
observeEvent(input$selected_learner, {
  reset_evaluation()
  eval_meta$current_learner <- get(input$selected_learner)$Learner$clone(deep = TRUE)
  eval_meta$current_learner$train(task = currenttask$task)
  reset_plots()
})

# save selected features and reset plot diagrams
observeEvent(input$feat_picker, {
  reset_plots()
  eval_meta$selected_features <- input$feat_picker
})

# get all possible features from the selected learner
get_feature_list <- function() {
  if (!is.null(input$selected_learner)) {
    ui <- wellPanel(
      tagList(
        fluidRow(
          column(
            12,
            h5("Selected Features: ")
          )
        ),
        fluidRow(
          column(
            12,
            pickerInput("feat_picker",
              choices = c(currenttask$featNames),
              multiple = TRUE,
              options = list(
                "actions-box" = TRUE,
                maxOptions = 6
              )
            )
          )
        ),
      )
    )
  }
}

# get all possible learners
get_learner_list <- function() {
  if (is.null(LearnerMeta$Learner_Avail)) {
    ui <- tagList(
      fluidRow(
        column(
          12,
          h5("No learner has been created yet. Please go to step 3 to define a learner.")
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
            choices = LearnerMeta$Learner_Avail,
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

# display a tabPanel with PDP and VI plots
display_plot_tabs <- function() {
  if (!is.null(eval_meta$feature_importance) || !is.null(eval_meta$feature_effect)) {
    output$eval_learner_plot_tabs <- renderUI({
      output$pdp_plot <- renderPlot({
        plot(eval_meta$feature_effect, features = eval_meta$selected_features)
      })
      output$vi_plot <- renderPlot({
        plot(eval_meta$feature_importance)
      })
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
    })
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