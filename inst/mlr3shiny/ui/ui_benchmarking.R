# Layout of benchmarking UI
tabpanel_benchmarking <- fluidPage(
  fluidRow(
    column(4,
           wellPanel(
             fluidRow(
               column(12,
                      h5("Create a Benchmark Design", style = "font-weight: bold;")
               )
             ),
             hr(style = "border-color: #3e3f3a;"),
             uiOutput(outputId = "Bench_learner_strats_selection"),
             fluidRow(
               column(12,
                      uiOutput(outputId = "Bench_overview")
               )
             ),
             fluidRow(
               column(12,
                      uiOutput(outputId = "Bench_download")
               )
             )
           )
    ),
    column(4,
           wellPanel(
             h5("Resampling Parameter Settings", style = "font-weight: bold;"),
             fluidRow(
               uiOutput(outputId = "Bench_params")
             ),
             uiOutput(outputId = "Bench_benchmark_button"),
             uiOutput(outputId = "Bench_measures")
           )
    ),
    column(4,
           hidden(
             wellPanel(
               id = "Bench_well_rslt",
               h5("Aggregated Benchmark Result", style = "font-weight: bold;"),
               DT::dataTableOutput(outputId = "Bench_rslt_view")
             )
           )
           )
  ),
)
