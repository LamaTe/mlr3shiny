tabpanel_evaluating <- fluidRow(
    column(
        3,
        wellPanel(
            uiOutput(outputId = "eval_learner_selection")
        ),
        fluidRow(
            column(
                12,
                uiOutput(outputId = "eval_learner_feat_selection")
            )
        )
    ),
    column(
        9,
        uiOutput(outputId = "eval_learner_plot_tabs")
    )
)