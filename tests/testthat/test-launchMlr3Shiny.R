library(shinytest)

test_that("launchMlr3Shiny works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  expect_pass(testApp("apps/full_workflow_test/", compareImages = FALSE))
})
