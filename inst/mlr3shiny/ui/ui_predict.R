# Layout of "Predict" UI
tabpanel_predict <- fluidPage(
  fluidRow(
    column(3,
           wellPanel(
             h5("Apply best learner on new data", style = "font-weight: bold;"),
             hr(style = "border-color: #3e3f3a;"),
             uiOutput(outputId = "Predict_learner_choice"),
             uiOutput(outputId = "Predict_learner_overview"),
             uiOutput(outputId = "Predict_learner_train_btn"),
             uiOutput(outputId = "Predict_learner_download_btn"),
             uiOutput(outputId = "Predict_codegen_btn")
           )
           ),
    column(3,
           wellPanel(
             h5("Import a new dataset", style = "font-weight: bold;"),
             hr(style = "border-color: #3e3f3a;"),
             selectInput(
               inputId = "Predict_data_type", label = h5("Type"),choices = c("csv", "txt", "xlsx","spss","sas","stata"),
               selected = "csv"),
             # To be expanded, if more file formats are accepted
             conditionalPanel(
               condition = "input.Predict_data_type == 'csv' || input.Predict_data_type == 'txt'",
               fileInput(inputId = "Predict_data_csv", label = h5("Select a File"),
                         accept = c("text/csv", ".csv", "text/comma-separated-values,text/plain", "text*")),
               checkboxInput("Predict_data_header", "Header", TRUE),
               selectInput(inputId = "Predict_data_sep", label = h5("Separator"),
                           choices = c(Comma = ",", Semicolon = ";", Tab = "\t", Space = " ", Vertical = "|")),
               selectInput(inputId = "Predict_data_quote", label = h5("Quote"),
                           choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
             ),
             conditionalPanel(
               condition = "input.Predict_data_type == 'xlsx' || input.Predict_data_type == 'xls'",
               fileInput(inputId = "Predict_data_xlsx", label = h5("Select a File"),
                         accept = c(".xlsx", ".xls")),
               checkboxInput("Predict_data_header_xlsx", "Header", TRUE),
               numericInput(inputId = "Predict_data_sheet", label = h5("Sheet"), value = 1)
             ),
             conditionalPanel(
               condition = "input.Predict_data_type == 'spss'",
               fileInput(
                 inputId = "Predict_data_spss", label = h5("Select a File"),
                 accept = c(".sav", ".por")
               )
             ),
             conditionalPanel(
               condition = "input.Predict_data_type == 'sas'",
               fileInput(
                 inputId = "Predict_data_sas", label = h5("Select a File"),
                 accept = c(".sas7bdat")
               )
             ),
             conditionalPanel(
               condition = "input.Predict_data_type == 'stata'",
               fileInput(
                 inputId = "Predict_data_stata", label = h5("Select a File"),
                 accept = c(".dta")
               )
             ),
             hr(style = "border-color: #3e3f3a;"),
             div(style = "display:inline-block; width:100%; text-align: center;",
                 actionButton(inputId = "Predict_predict", label = "Predict target", icon = icon("search"))
             )
           )
           ),
    column(6,
           hidden(
             wellPanel(
               id = "Pred_well_new_data",
               h5("New dataset", style = "font-weight: bold;"),
               DT::dataTableOutput(outputId = "Pred_new_data_view")
             )
           ),
           hidden(
             wellPanel(
               id = "Pred_well_prediction",
               h5("Prediction", style = "font-weight: bold;"),
               DT::dataTableOutput(outputId = "Pred_prediction_view"),
               fluidRow(
                 column(12,
                        downloadButton(outputId  = "Pred_prediction_download_csv", label = "Export as .csv", style = "float: left;"),
                        downloadButton(outputId  = "Pred_prediction_download_rds", label = "Export as .rds", style = "float: right;")
                        )
                 )
               )
             ),
             hidden(
              wellPanel(
              id = "Pred_well_decision_tree",
              fluidRow(
                column(4, h5("Decision Tree", style="font-weight: bold;")),
                column(4, actionButton(inputId = "action_visualize_predict", label = "Visualize Tree", icon = icon("hammer")))),
              uiOutput(outputId = "plotDecisionTree")
              )
             )
    )
  )
)
