app <- ShinyDriver$new("../", seed = 42, loadTimeout = 1e+05, shinyOptions = list(port = 8000))
app$snapshotInit("mytest")

app$snapshot()
