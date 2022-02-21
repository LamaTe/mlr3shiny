server <- function(input, output, session) {
  server_files = list.files(path = "./server", pattern = "*.R")
  server_files = paste0("server/", server_files)
  for (i in seq_along(server_files)) {
    source(server_files[i], local = TRUE)
  }
  options(shiny.maxRequestSize=1000*1024^2)
  observeEvent(input$Help, {
    if (req(input$navbar) == "1. Data") {
      showModal(
        modalDialog(
          title = h2("Data Import & General Information", style = "text-align: center;"),
          h4("General:"),
          HTML(userhelp[["Data"]][1]),
          h4("Description:"),
          HTML(userhelp[["Data"]][2]),
          easyClose = TRUE,
          footer = div(style = "display:inline-block;width:100%;text-align: center;",
                       modalButton('OK'))
          )
      )
    }
    else if (req(input$navbar) == "2. Task") {
      showModal(
        modalDialog(
          title = h2("Task Creation", style = "text-align: center;"),
          h4("Description:"),
          HTML(userhelp[["Task"]][1]),
          hr(),
          h4("Functionalities:"),
          HTML(userhelp[["Task"]][2]),
          easyClose = TRUE,
          footer = div(style = "display:inline-block;width:100%;text-align: center;",
                       modalButton('OK')))
      )
    }
    else if (req(input$navbar) == "3. Learner") {
      showModal(
        modalDialog(
          title = h2("Learner Selection", style = "text-align: center;"),
          h4("Description:"),
          HTML(userhelp[["Learner"]][1]),
          hr(),
          h4("Functionalities:"),
          HTML(userhelp[["Learner"]][2]),
          HTML("</br>", "</br>"),
          HTML(userhelp[["Learner"]][3]),
          HTML("</br>", "</br>"),
          HTML(userhelp[["Learner"]][4]),
          HTML("<br/>", "<br/>"),
          HTML(userhelp[["Learner"]][5]),
          HTML("<br/>", "<br/>"),
          HTML(userhelp[["Learner"]][6]),
          easyClose = TRUE,
          footer = div(style = "display:inline-block;width:100%;text-align: center;",
                       modalButton('OK')))
      )
    }
    else if (req(input$navbar) == "4. Train & Evaluate") {
      switch(Help$Tracker,
             {showModal(
               modalDialog(
                 title = h2("Basic Model Training & Evaluation", style = "text-align: center;"),
                 h4("Description:"),
                 HTML(userhelp[["Evaluate"]][1]),
                 hr(),
                 h4("Functionalities:"),
                 HTML(userhelp[["Evaluate"]][2]),
                 easyClose = TRUE,
                 footer = div(style = "display:inline-block;width:100%;text-align: center;",
                              modalButton('OK')))
             )},
             {
               showModal(
                 modalDialog(
                   title = h2("Resampling", style = "text-align: center;"),
                   h4("Description:"),
                   HTML(userhelp[["Resample"]][1]),
                   hr(),
                   h4("Functionalities:"),
                   HTML(userhelp[["Resample"]][2]),
                   HTML("</br>", "</br>"),
                   HTML(userhelp[["Resample"]][3]),
                   HTML("</br>", "</br>"),
                   HTML(userhelp[["Resample"]][4]),
                   HTML("<br/>", "<br/>"),
                   HTML(userhelp[["Resample"]][5]),
                   easyClose = TRUE,
                   footer = div(
                     style = "display:inline-block;width:100%;text-align: center;",
                     modalButton('OK')
                   )
                 )
               )
               }
             )
    }
    else if (req(input$navbar) == "5. Benchmarking") {
      showModal(
        modalDialog(
          title = h2("Benchmarking", style = "text-align: center;"),
          h4("Description:"),
          HTML(userhelp[["Benchmark"]][1]),
          hr(),
          h4("Functionalities:"),
          HTML(userhelp[["Benchmark"]][2]),
          easyClose = TRUE,
          footer = div(style = "display:inline-block;width:100%;text-align: center;",
                       modalButton('OK')))
      )
    }
    else if (req(input$navbar) == "6. Predict") {
      showModal(
        modalDialog(
          title = h2("Prediction For New Data", style = "text-align: center;"),
          h4("Description:"),
          HTML(userhelp[["Predict"]][1]),
          hr(),
          h4("Functionalities:"),
          HTML(userhelp[["Predict"]][2]),
          easyClose = TRUE,
          footer = div(style = "display:inline-block;width:100%;text-align: center;",
                       modalButton('OK')))
      )
    }
    else if (req(input$navbar) == "7. Explain") {
      showModal(
        modalDialog(
          title = h2("Explain your Learners", style = "text-align: center;"),
          h4("Description:"),
          HTML(userhelp[["Explain"]][1]),
          hr(),
          h4("Functionalities:"),
          HTML(userhelp[["Explain"]][2]),
          easyClose = TRUE,
          footer = div(style = "display:inline-block;width:100%;text-align: center;",
                       modalButton('OK')))
      )
    }
  })
}
