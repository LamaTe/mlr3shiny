# Layout for "Task" UI
tabpanel_Task <- fluidPage(
  fluidRow(
    column(6,
           wellPanel(
             fluidRow(
               column(4,
                      h5("Define a Task", style = "font-weight: bold;"),
                      hr(style = "border-color: #3e3f3a;"),
                      selectInput(inputId = "Task_backend", label = h5("Select Data Backend"),
                                  choices = list( "imported training data", examples = c("iris", "mtcars")),
                                  selected = "iris", multiple = FALSE),
                      uiOutput(outputId = "Task_make_id"),
                      uiOutput(outputId = "Task_make_target"),
                      uiOutput(outputId = "Task_make_task")
               ),
               column(8,
                      tableOutput(outputId = "Task_overview")
                     )
             )
           )
           ),
    column(6,
           wellPanel(
             uiOutput(outputId = "Task_processing")
           )
    )
  )
)
