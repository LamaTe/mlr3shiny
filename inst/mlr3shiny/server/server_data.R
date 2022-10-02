# generate data.table when a fitting file is uploaded
data <- reactiveValues(traindata = NULL)

### TO-DO make sure that missings are loaded as na to be counted correctly later on

# training data
observe({
  if (input$Data_train_type == "csv" || input$Data_train_type == "txt") {
     filepath <-  input$Data_train_csv$datapath
     if (!is.null(filepath) && (str_sub(filepath, -4, -1) == ".csv" || (str_sub(filepath, -4, -1) == ".txt" ))) {
       data$traindata <- read.csv(file = filepath, header = input$Data_train_header,
                               sep = input$Data_train_sep, quote = input$Data_train_quote, stringsAsFactors = TRUE)
     }
  } else if (input$Data_train_type == "xlsx") {
      filepath <-  input$Data_train_xlsx$datapath
        if (!is.null(filepath) && (str_sub(filepath, -5, -1) == ".xlsx")) {
         traintibble <- read_excel(path = filepath, col_names = input$Data_train_header_xlsx,
                                         sheet = input$Data_train_sheet)
         traindf <- as.data.frame(traintibble)
         data$traindata <- modify_at(traindf,
                                     which(as.character(sapply(traindf, class)) == "character"),
                                     as.factor)
        }
  } else if (input$Data_train_type == "spss") {
    filepath <-  input$Data_train_spss$datapath
    if (!is.null(filepath) && (str_sub(filepath, -4, -1) == ".sav" | str_sub(filepath, -4, -1) == ".por") ) {
      traintibble <- read_spss(file = filepath)
      traindf <- as.data.frame(traintibble)
      traindf <- as_factor(traindf)
      data$traindata <- modify_at(traindf,
                                  which(as.character(sapply(traindf, class)) == "character"),
                                  as.factor)
    }
  } else if (input$Data_train_type == "sas") {
    filepath <-  input$Data_train_sas$datapath
    if (!is.null(filepath) && (str_sub(filepath, -9, -1) == ".sas7bdat") ) {
      traintibble <- read_sas(data_file = filepath)
      traindf <- as.data.frame(traintibble)
      data$traindata <- modify_at(traindf,
                                  which(as.character(sapply(traindf, class)) == "character"),
                                  as.factor)
    }
  } else if (input$Data_train_type == "stata") {
    filepath <-  input$Data_train_stata$datapath
    if (!is.null(filepath) && (str_sub(filepath, -4, -1) == ".dta") ) {
      traintibble <- read_stata(file = filepath)
      traindf <- as.data.frame(traintibble)
      data$traindata <- modify_at(traindf,
                                  which(as.character(sapply(traindf, class)) == "character"),
                                  as.factor)
    }
  }
})

output$Data_train_view <- DT::renderDataTable({
  DT::datatable(data$traindata, options = list(scrollX = TRUE))
})

observe({
  toggle(id = "Data_train_view", condition = (!is.null(data$traindata)))
})


# show help if needed
observeEvent(input$User_help, {
  shinyalert(title = "Data Import", text = userhelp[["Data"]], closeOnClickOutside = TRUE, animation = FALSE)
})

