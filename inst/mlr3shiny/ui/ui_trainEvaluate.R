# Layout of "Train and Evaluate" UI
tabpanel_TrainFit <- fluidPage(
  fluidRow(
    id = "TrainFit_Basic",
    column(4,
           wellPanel(
                fluidRow(
                  column(6,
                         h5("Basic Workflow", style = "font-weight: bold;")
                         ),
                  column(6,
                         actionButton(inputId = "TrainFit_resample", label = "Use Resampling")
                  )
                  ),
                  hr(style = "border-color: #3e3f3a;"),
                  fluidRow(
                           uiOutput(outputId = "TrainFit_learner_selection")
                  ),
                  hr(style = "border-color: #3e3f3a;"),
                  fluidRow(
                    column(12,
                           uiOutput(outputId = "TrainFit_overview")
                           )
                  )

                )

           ),
    column(4,
           wellPanel(
             h5("Train Model", style = "font-weight: bold;"),
             uiOutput(outputId = "TrainFit_train")
             ),
           wellPanel(
             h5("Predict Target Variable", style = "font-weight: bold;"),
             uiOutput(outputId = "TrainFit_predict")
             ),
           wellPanel(
             h5("Measure Prediction Performance", style = "font-weight: bold;"),
             uiOutput(outputId = "TrainFit_score")
           )
    ),
    column(4,
           hidden(
             wellPanel(
               id = "TrainFit_well_test",
               h5("Predicted Target on Test Data", style = "font-weight: bold;"),
               DT::dataTableOutput(outputId = "TrainFit_pred_view_test")
               )
           ),
           hidden(
             wellPanel(
               id = "TrainFit_well_train",
               h5("Predicted Target on Training Data", style = "font-weight: bold;"),
               DT::dataTableOutput(outputId = "TrainFit_pred_view_train")
               )
           )
    )
  ),
  hidden(resample_ui)
)
