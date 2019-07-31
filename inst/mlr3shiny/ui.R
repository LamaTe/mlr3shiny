
### aus shinymlr
ui_files <- list.files(path = "./ui", pattern = "*.R")
ui_files <- paste0("ui/", ui_files)

for (i in seq_along(ui_files)) {
  source(ui_files[i], local = TRUE)
}
###

ui <- tagList(
  useShinyjs(),
  useShinyalert(),
  circleButton(
    inputId = "Help", icon = icon("question"),
    status = "white", size = "sm", style = "position:fixed; right:2em; top:0.7em; z-index:1001"
  ),
  navbarPage(
    theme = shinytheme("sandstone"),
    title = a("mlr3shiny", href = "https://github.com/LamaTe/mlr3shiny", target = "_blank", style = "color: white;"),
    windowTitle = "mlr3shiny",
    id = "navbar",
    tabPanel("1. Data",
      tabpanel_Data,
      icon = icon("database")
    ),
    tabPanel("2. Task",
      tabpanel_Task,
      icon = icon("bookmark")
    ),
    tabPanel("3. Learner",
      tabpanel_Learner,
      icon = icon("filter")
    ),
    tabPanel("4. Train & Evaluate",
      tabpanel_TrainFit,
      icon = icon("cogs")
    ),
    tabPanel("5. Benchmarking",
      tabpanel_benchmarking,
      icon = icon("chart-bar")
    ),
    tabPanel("6. Predict",
      tabpanel_predict,
      icon = icon("search")
    )
  )
)
