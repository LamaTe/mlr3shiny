library(shinytest)

test_that("mlr3shiny app works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  collation <- Sys.getlocale("LC_COLLATE")
  Sys.setenv(LC_COLLATE = "en_US.UTF-8")
  Sys.setlocale("LC_COLLATE", "en_US.UTF-8")

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  appdir <- system.file(package = "mlr3shiny", "mlr3shiny")
  expect_pass(testApp(appdir, compareImages = FALSE))
  Sys.setlocale("LC_COLLATE", collation)
})
