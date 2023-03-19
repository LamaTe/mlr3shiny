# reactive values for the chosen learner, the features selected and the iml objects used for explaining the model
eval_meta <- reactiveValues(current_learner = NULL, 
                            selected_features = NULL, 
                            feature_effect = NULL, 
                            feature_importance = NULL, 
                            feature_importance_compare = NULL)

# shiny outputs for the ui
output$eval_learner_selection <- renderUI({
  get_learner_selection()
})

output$eval_explanation_selection <- renderUI({
  get_explanation_selection()
})

output$eval_loss_function_selection <- renderUI({
  loss_ui_builder_new(currenttask$task, input$selected_learner)
})

output$eval_compare_method_selection <- renderUI({
  get_compare_method_list()
})

output$eval_automation_slider <- renderUI({
  compute_slider_values()
})

output$eval_analysis_plot_selection <- renderUI({
  get_analysis_plot()
})

output$eval_analysis_automation <- renderUI({
get_selection_automation()  
})

output$eval_learner_feat_selection <- renderUI({
  get_feature_selection()
})

output$eval_learner_plot_tabs <- renderUI({
  display_plot_tabs()
})

##display warning message if model interpretation may fail and how the user can avoid this
observeEvent(input$explanation_selection, {
  if(isTRUE(currenttask$task$properties == "multiclass" && eval_meta$current_learner$predict_type == "response")){
     showNotification("This learner may not work due to the predict_type being set 
                      to response. Consider changing it to 'prob' in the learner-tab", duration = 15, type = "warning")
    }
  
})


# observe "start evaluation" button and start DALEX Workflow
observeEvent(input$evaluate_start, {
  withProgress(message = "Initialising evaluation", style = "notification",
      withCallingHandlers(
      tryCatch({
          
        # this condition starts a seperate preparatory workflow for twoclass modells
        # this is due to an mlr3 pipeOps syntax restriction
        if (isTRUE(currenttask$task$properties == "twoclass") ){
          #Preparation Learner
          if(grepl("classif.ranger", eval_meta$current_learner$id , fixed = TRUE)){
            twoclass_learner <- eval_meta$current_learner$graph_model$pipeops$classif.ranger$learner_model
          }
          else if(grepl("classif.rpart", eval_meta$current_learner$id , fixed = TRUE) ){
            twoclass_learner <- eval_meta$current_learner$graph_model$pipeops$classif.rpart$learner_model
          }
          #Error: <TaskClassif:german_credit> has the following unsupported feature types: factor, ordered
          # SVM and xgboost get their feature types changed to compatible ones --> This does not happen here yet
          else if(grepl("classif.svm", eval_meta$current_learner$id , fixed = TRUE) ){
            twoclass_learner <- eval_meta$current_learner$graph_model$pipeops$classif.svm$learner_model
            twoclass_learner = po("encode") %>>% twoclass_learner
            }
          #Error: <TaskClassif:german_credit> has the following unsupported feature types: factor, ordered
          else if(grepl("classif.xgboost", eval_meta$current_learner$id , fixed = TRUE) ){
            twoclass_learner <- eval_meta$current_learner$graph_model$pipeops$classif.xgboost$learner_model
          }
          else if(grepl("classif.log_reg", eval_meta$current_learner$id , fixed = TRUE) ){
            twoclass_learner <- eval_meta$current_learner$graph_model$pipeops$classif.log_reg$learner_model
          }
         
          #train the extracted learner again
          twoclass_learner$train(currenttask$task)
     
          #Preparation for Explainer | TASK
          dalex_temp <- currenttask$task$data(data_format = "data.table")
         
          dalex_predictors <- dalex_temp %>% select(-currenttask$task$target_names)
          dalex_target <- dalex_temp %>% select(currenttask$task$target_names)
          colnames(dalex_target) <- "target"
        
          dalex_target$target <- as.character(dalex_target$target)
          dalex_target$target[dalex_target$target == currenttask$task$positive] <- "1"
          dalex_target$target[dalex_target$target != "1"] <- "0"
          
          incProgress(0.1, paste("Initialising Workflow"))
          
          #creating the DALEX(tra) Explainer object for further computations
          
          model <- explain_mlr3(twoclass_learner, 
                                data = dalex_predictors, 
                                y = as.numeric(dalex_target$target),
                                predict_function_target_column = currenttask$positive,
                                verbose = FALSE) 
          
          
          incProgress(0.2, paste("Creating Explainer"))
         
        }
      
        #starting the other workflow for multiclass/ regression models
        else{
        dalex_temp <- currenttask$task$data(data_format = "data.table")
        dalex_predictors <- dalex_temp %>% select(-currenttask$task$target_names)
        dalex_target <- dalex_temp %>% select(currenttask$task$target_names)
         
        #This does not work unfortunately
        #if(isTRUE(currenttask$task$properties == "multiclass" && eval_meta$current_learner$predict_type == "response")){
         # eval_meta$current_learner$predict_type <- "prob"
        #}
        
        incProgress(0.1, paste("Initialising Workflow"))
        
        #creating the DALEX(tra) Explainer object for further computations
        
        model <- explain_mlr3(eval_meta$current_learner, 
                         data = dalex_predictors, 
                         y = dalex_target,
                         verbose = FALSE)
                         
        
        incProgress(0.2, paste("Creating Explainer"))
        }
        
        # Computing Results based on the User Input
        if("Feature importance" %in% input$explanation_selection){
          incProgress(0.4, paste("Computing feature importance"))  
          eval_meta$feature_importance <- model_parts(
                model, 
                loss_function = match.fun(input$loss_picker), 
                type = input$compare_picker)
            plot_feature_importance()
        }
        
        
        if("Specific feature analysis" %in% input$explanation_selection && input$automation_flag == "manual"){
          incProgress(0.6, paste("Computing plot: ", input$method_picker))  
          eval_meta$feature_effect <- model_profile(
                model, 
                variables = eval_meta$selected_features,
                type = input$method_picker)
            plot_feature_analysis(eval_meta$feature_effect)
        }
        
        if("Specific feature analysis" %in% input$explanation_selection && input$automation_flag == "automatic"){
          incProgress(0.6, paste("Computing plot: ", input$method_picker))
          #temp_var_imp <- model %>% model_parts(B=1)
          #temp_var_imp_vector <- temp_var_imp$variable
          
          #eval_meta$feature_effect <- model_profile(
             # model, 
            #  variables <<- head(tail(temp_var_imp_vector,5),4),
           #   type = input$method_picker)
          #plot_feature_analysis(eval_meta$feature_effect)
          
          
          #1. Compute VI in a time efficient manner
          temp_var_imp <- model %>% model_parts(B=1)
          #2. Get Vector of most important columns from worst to best
          temp_var_imp_vector <- temp_var_imp$variable
          #3. Remove Target
          temp_var_imp_vector <- temp_var_imp_vector[!temp_var_imp_vector == currenttask$task$target_names]
          #4. Remove Base_line and __full_model__
          len <- length(temp_var_imp_vector)
          temp_var_imp_vector <- head(tail(temp_var_imp_vector,len-1), len-2)
          
          #compute Feature Effect
          eval_meta$feature_effect <- model_profile(
            model, 
            variables <- tail(temp_var_imp_vector, as.numeric(input$automation_slider)),
            type = input$method_picker)
          plot_feature_analysis(eval_meta$feature_effect)
        }
        incProgress(0.8, paste("Finishing Plot"))
        
        
      },
      error = errorAlertTrain
      )
    )
  )
})

# observe chosen learner in ui and change meta object
observeEvent(input$selected_learner, {
  reset_evaluation()
  eval_meta$current_learner <- trained_learner_list[[input$selected_learner]]
  # showing only relevant loss functions
  reset_plots()
})

# save selected features and reset plot diagrams
observeEvent(input$feat_picker, {
  eval_meta$selected_features <- input$feat_picker
})

#builder for explanation selection
get_explanation_selection <- function() {
  if (!is.null(input$selected_learner)) {
    ui <- tagList(
      fluidRow(
        column(
          12,
          h5("Choose explanation method", style = "font-weight: bold;"),
          hr(style = "border-color: #3e3f3a;"),
        ),
        column(
          12,
          checkboxGroupInput(inputId = "explanation_selection", 
                             label = NULL,
                             choices = c("Feature importance", 
                                         "Specific feature analysis"),
                             selected = c("Feature importance", 
                                          "Specific feature analysis")
                             )
        )
      )
    )
    return(ui)
    
  }
  
}

# builder for compare method selection
get_compare_method_list <- function() {
  if (!is.null(input$selected_learner)) {
    if("Feature importance" %in% input$explanation_selection){
    ui <- tagList(
      
      fluidRow(
        column(
          12,
          h5("Select compare method for Feature Importance:")
        ),
        column(
          12,
          pickerInput("compare_picker",
            choices = c("ratio", "raw")
          )
        )
      )
    )
    return(ui)}
  }
}


#UI Builder for loss_function selection
loss_ui_builder_new <- function(task, learner) {
  if (!is.null(input$selected_learner)) {
    if("Feature importance" %in% input$explanation_selection){
  ui <- tagList(
    fluidRow(
      column(
        12,
        h5("Feature importance", style = "font-weight: bold;"),
        hr(style = "border-color: #3e3f3a;"),
      )
    ),
    fluidRow(
      column(
        12,
        h5("Select loss function for Feature Importance: "),
      )
    ),
    if(task$task_type == "classif"){
      if (task$properties == "twoclass"){
        fluidRow(
        column(
        12,
        pickerInput("loss_picker",
                   choices = c("One-Minus-AUC" = "loss_one_minus_auc")
          )
         )
        )
      }
      else{
        fluidRow(
          column(
            12,
            pickerInput("loss_picker",
                        choices = c("Cross-Entropy (Log loss)" = "loss_cross_entropy"
                                    )
            )
          )
        )
      }
                                    }
    
   
    else if(task$task_type == "regr"){
      fluidRow(
        column(
          12,
          pickerInput("loss_picker",
                      choices = c(
                        "Sum of Squared (SSE)" = "loss_sum_of_squares",
                        "Root Mean Square (RMSE)" = "loss_root_mean_square"
                      )
          )
        )
      ) 
      
    }
  )
  return(ui)
    }
  }
  }

#compute slider values
compute_slider_values <- function(){
  if (!is.null(input$selected_learner)) {
    if("Specific feature analysis" %in% input$explanation_selection){
      if("automatic" %in% input$automation_flag){
        
        #This function used to be longer... found a better solution
        #This function is now pretty much redundant
        col_names_vector <- features_to_use$features
        len <- length(col_names_vector)
        
        ui <- automation_slider_builder(len)  
  
      }}}

}

#build the slider for automatic model_profile
automation_slider_builder <- function(vector_length){
  
      ui <- tagList( 
        fluidRow(
          column(
            12,
            h5("Choose how many features you want to display:"),
          ),
          column(
            12,
            h5("The actual amount of features displayed may vary."),
          ),
          column(
            12,
            sliderInput("automation_slider",label = "", step = 1, min = 1, 
                        max = vector_length, value = 1)
          
        )
       )
      )
    

  }

#get the plot information
get_analysis_plot <- function(){
  if (!is.null(input$selected_learner)) {
  if("Specific feature analysis" %in% input$explanation_selection){
   ui <- tagList( 
     fluidRow(
       column(
         12,
         h5("Specific feature analysis", style = "font-weight: bold;"),
         hr(style = "border-color: #3e3f3a;"),
       )
     ),
     fluidRow(
       column(
         12,
         h5("Select plot to display: "),
       )
     ),
     fluidRow(
      column(
        12,
        pickerInput("method_picker",
                    choices = c("PD-plot" ="partial", 
                                "ALE-Plot" = "accumulated",
                                "ICE-plot" = "conditional"),
                    
        )
      )
    )
   )
  }
  }
}


# get all possible features from the selected learner
get_feature_selection <- function() {
  if (!is.null(input$selected_learner)) {
  if("Specific feature analysis" %in% input$explanation_selection){
    if("manual" %in% input$automation_flag){
  ui <- tagList(
    fluidRow(
      column(
        12,
        h5("Select Features to show in Feature-Analysis-Plot:")
      )
    ),
    fluidRow(
      column(
        12,
        pickerInput("feat_picker",
          choices = c(currenttask$featNames),
          multiple = TRUE
        )
      )
    ),
  )
}}}}

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
  } else {
    ui <- tagList(
      fluidRow(
        column(
          12,
          h5("Explain your model", style = "font-weight: bold;"),
          hr(style = "border-color: #3e3f3a;"),
        ),
        column(
          12,
          radioButtons(
            inputId = "selected_learner",
            label = h5("Select a learner to evaluate its performance."),
            choices = get_trained_learners(),
            selected = character(0)
          )
        )
      )
    )
    return(ui)
  }
}

# printing the pdp and vi plot to output
calculate_plots <- function(x) {
  output$pdp_plot <- renderPlot({
    plot(x, geom = "aggregates")
  })
  output$vi_plot <- renderPlot({
    plot(eval_meta$feature_importance)
  })
}

#plot function for feature importance
plot_feature_importance <- function(){
  output$feature_imp_plot <- renderPlot({
    plot(eval_meta$feature_importance)
  })
  
}

#plot function for feature analysis
plot_feature_analysis <- function(x){
  output$feature_analysis_plot <- renderPlot({
    plot(x, geom = "aggregates")
  })
  #Optional future feature: Explanation for Automatic display
  #if("Specific feature analysis" %in% input$explanation_selection && input$automation_flag == "automatic"){
  #output$auto_response <- renderText({
   # paste("The following features were selected for display: ")
    
  #})
  #output$auto_response2 <- renderText({
   # paste(variables)
  #}
  
  #)
}

#display option to automate feature analysis or make it manual

get_selection_automation <- function(){
  if("Specific feature analysis" %in% input$explanation_selection){
    ui <- tagList(
        column(
          12,
          radioButtons(
            inputId = "automation_flag",
            label = h5("Choose method for analysis"),
            choices = c("manual", "automatic")
          )
        )
      )
    return(ui)
    
  }
  
}

# display a tabPanel for the PDP and VI plots
display_plot_tabs <- function() {
  #if Feature imp is selected
  if (!is.null(eval_meta$feature_importance) && is.null(eval_meta$feature_effect) && !is.null(input$selected_learner)) {
    ui <- tabsetPanel(
      type = "tabs",
      tabPanel(
        "Feature Importance",
        wellPanel(
          plotOutput(outputId = "feature_imp_plot")
        )
      )
    )
    return(ui)
  }
  #if feature analysis is selected
  if (is.null(eval_meta$feature_importance) && !is.null(eval_meta$feature_effect) && !is.null(input$selected_learner)) {
    ui <- tabsetPanel(
      type = "tabs",
      tabPanel(
        "Feature-Analysis-Plot",
        wellPanel(
          plotOutput(outputId = "feature_analysis_plot")
        )
        #textOutput(outputId = "auto_response"),
       # textOutput(outputId = "auto_response2")
        )
    )
    return(ui)
  }
  #if both are selected
  if (!is.null(eval_meta$feature_importance) && !is.null(eval_meta$feature_effect) && !is.null(input$selected_learner)) {
    ui <- tabsetPanel(
      type = "tabs",
      tabPanel(
        "Feature Importance",
        wellPanel(
          plotOutput(outputId = "feature_imp_plot")
        )
      ),
      tabPanel(
        "Feature-Analysis-Plot",
        wellPanel(
          plotOutput(outputId = "feature_analysis_plot")
        )#,
       # textOutput(outputId = "auto_response")
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
  output$feature_analysis_plot <- NULL
  output$feature_imp_plot <- NULL
  output$auto_response <- NULL
  output$auto_response2 <- NULL
}

observe({
  if (!is.null(input$selected_learner)) {
    show("feature_selection_panel")
  }
})

# disabling the start button if no feature is selected
observe({
  if (length(input$feat_picker) == 0 &&"Specific feature analysis" %in% input$explanation_selection && "automation_flag" == "manual") {
    disable("evaluate_start")
  } else {
    enable("evaluate_start")
  }
})

