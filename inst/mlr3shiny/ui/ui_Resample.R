# Layout of "Resampling" UI
resample_ui <- fluidRow(
  id = "TrainFit_Resample",
  column(4,
         wellPanel(
           fluidRow(
             column(6,
                    h5("Resampling", style = "font-weight: bold;")
             ),
             column(6,
                    actionButton(inputId = "TrainFit_Basic", label = "Back to Basic")
             )
           ),
           hr(style = "border-color: #3e3f3a;"),
           uiOutput(outputId = "Res_learner_selection"),
           fluidRow(
             column(12,
                    uiOutput(outputId = "Res_overview")
             )
           ),
           fluidRow(
             column(12,
                    uiOutput(outputId = "Res_download")
             )
           )
        )
  ),
  column(4,
         wellPanel(
           h5("Resampling Parameter Settings", style = "font-weight: bold;"),
           fluidRow(
             uiOutput(outputId = "Res_params")
           ),
           uiOutput(outputId = "Res_resample_button"),
           uiOutput(outputId = "Res_measure")
         )
         ),
  column(4,
         hidden(
           wellPanel(
             id = "Res_well_prediction",
             h5("Combined Predicitons of Target Variable", style = "font-weight: bold;"),
             DT::dataTableOutput(outputId = "Res_pred_view")
           )
         )
  )
)
