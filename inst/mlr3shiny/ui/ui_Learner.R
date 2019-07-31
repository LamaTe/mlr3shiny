tabpanel_Learner <- fluidPage(
    fluidRow(
      column(4,
             wellPanel(
               h5("Select a machine-learning algorithm", style = "font-weight: bold;"),
               hr(style = "border-color: #3e3f3a;"),
               fluidRow(
                 column(12,
                        fluidRow(
                          column(12,
                                 uiOutput(outputId = "Learner_other_Learner1")
                                 )
                          ),
                        fluidRow(
                          column(12,
                                 uiOutput(outputId = "Learner_other_Learner2")
                                 )
                          ),
                        fluidRow(
                          column(12,
                                 uiOutput(outputId = "Learner_other_Learner3")
                          )
                        ),
                        fluidRow(
                          column(12,
                                 uiOutput(outputId = "Learner_other_Learner4")
                          )
                        ),
                        fluidRow(
                          column(12,
                                 uiOutput(outputId = "Learner_other_Learner5")
                          )
                        ),
                        fluidRow(
                          column(12,
                                 uiOutput(outputId = "Learner_other_Learner6")
                          )
                        ),
                        fluidRow(
                          column(12,
                                 uiOutput(outputId = "Learner_other_Learner7")
                          )
                        ),
                        HTML("<br/>"),
                        fluidRow(
                          column(12,
                                 div(style = "display:inline-block; width:100%; text-align: center;", 
                                     actionButton(inputId = "Learner_add", label = " Add Learner", icon = icon("plus")))
                          )
                        )
                 )
               )
             )
      ),
     column(8,
                   id = "Learner_NA",
                   wellPanel(h4("No Learner has been created yet.", 
                                style = "text-align: center; text-weight: bold;"))
     ),
     hidden(
       mainPanel(
         id = "Learner_Learners",
         tabsetPanel(
           id = "Learner_Learners_Tab",
           tabPanel(title = "Learner 1",
                    uiOutput(outputId = "Learner1_ov"),
                    uiOutput(outputId = "Learner1_tab")
           ),
           tabPanel(title = "Learner 2",
                    uiOutput(outputId = "Learner2_ov"),
                    uiOutput(outputId = "Learner2_tab")),
           tabPanel(title = "Learner 3",
                    uiOutput(outputId = "Learner3_ov"),
                    uiOutput(outputId = "Learner3_tab")),
           tabPanel(title = "Learner 4",
                    uiOutput(outputId = "Learner4_ov"),
                    uiOutput(outputId = "Learner4_tab")),
           tabPanel(title = "Learner 5",
                    uiOutput(outputId = "Learner5_ov"),
                    uiOutput(outputId = "Learner5_tab")),
           tabPanel(title = "Learner 6",
                    uiOutput(outputId = "Learner6_ov"),
                    uiOutput(outputId = "Learner6_tab")),
           tabPanel(title = "Learner 7",
                    uiOutput(outputId = "Learner7_ov"),
                    uiOutput(outputId = "Learner7_tab"))
           )
         )
       )
     )
  )
