ui_files <- list.files(path = "./ui", pattern = "*.R")
ui_files <- paste0("ui/", ui_files)

for (i in seq_along(ui_files)) {
  source(ui_files[i], local = TRUE)
}

ui <- tagList(
  tags$head(
    tags$style(
      HTML(
        ".well {box-shadow: 0 2px 3px 0
            rgba(90,97,105,.11), 0 4px 8px 0
            rgba(90,97,105,.12),0 15px 22px 0
            rgba(90,97,105,.1),0 7px 35px 0
            rgba(90,97,105,.1) !important;
            background-color: #fff !important;}
          ",
        ".navbar {background-color: #2FA4E7;}",
        ".navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover
        {background-color:}",
        "h5 {color: black;}"
      )
    )
  ),
  useShinyjs(),
  circleButton(
    inputId = "Help", icon = icon("question"),
    status = "white", size = "sm", style = "position:absolute; right:2em; top:0.35em; z-index:1001; color: #2FA4E7;"
  ),
  navbarPage(
    theme = shinythemes('cerulean'),
    title = a("mlr3shiny", href = "https://github.com/LamaTe/mlr3shiny", target = "_blank", style = "color: white;"),
    windowTitle = "mlr3shiny",
    id = "navbar",
    tabPanel("1. Data",
      tabpanel_Data,
      icon = icon("database"),
      id = 'data'
    ),
    tabPanel("2. Task",
      tabpanel_Task,
      icon = icon("bookmark"),
      id = 'task'
    ),
    tabPanel("3. Learner",
      tabpanel_Learner,
      icon = icon("filter"),
      id = 'learner'
    ),
    tabPanel("4. Train & Evaluate",
      tabpanel_TrainFit,
      icon = icon("cogs"),
      id = 'train'
    ),
    tabPanel("5. Benchmarking",
      tabpanel_benchmarking,
      icon = icon("chart-bar"),
      id = 'benchmark'
    ),
    tabPanel("6. Predict",
      tabpanel_predict,
      icon = icon("search"),
      id = 'predict'
    ),
    tabPanel("7. Explain",
    tabpanel_evaluating,
    icon = icon("chalkboard-teacher")
    )
  )
)
