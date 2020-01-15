# Layout of  "Import" tab
tabpanel_Data <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # import training data
      helpText("Welcome to mlr3shiny!", style = "text-align: center;"),
      helpText("Get to know machine learning in R with mlr3 and the help of Shiny.", HTML("<br/>"),
               "For more information click on the questionmark in the top-right corner or visit the GitHub repository by clicking on 'mlr3shiny'.
                To get started import a data set.",
               style = "text-align: center;"),
      hr(style = "border-color: #3e3f3a;"),
      h5("Import training data", style = "font-weight: bold;"),
      selectInput(
        inputId = "Data_train_type", label = h5("Type"), choices = c("csv", "txt", "xlsx"),
        selected = "csv"
      ),
      # To be expanded, if more file formats are accepted
      conditionalPanel(
        condition = "input.Data_train_type == 'csv' || input.Data_train_type == 'txt'",
        fileInput(
          inputId = "Data_train_csv", label = h5("Select a File"),
          accept = c("text/csv", ".csv", "text/comma-separated-values,text/plain", "text*")
        ),
        checkboxInput("Data_train_header", "Header", TRUE),
        selectInput(
          inputId = "Data_train_sep", label = h5("Separator"),
          choices = c(Comma = ",", Semicolon = ";", Tab = "\t", Space = " ", Vertical = "|"),
          selected = ","
        ),
        selectInput(
          inputId = "Data_train_quote", label = h5("Quote"),
          choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'
        )
      ),
      conditionalPanel(
        condition = "input.Data_train_type == 'xlsx' || input.Data_train_type == 'xls'",
        fileInput(
          inputId = "Data_train_xlsx", label = h5("Select a File"),
          accept = c(".xlsx", ".xls")
        ),
        checkboxInput("Data_train_header_xlsx", "Header", TRUE),
        numericInput(inputId = "Data_train_sheet", label = h5("Sheet"), value = 1)
      )
    ),
    mainPanel(
      width = 9,
      DT::dataTableOutput(outputId = "Data_train_view")
    )
  )
)
