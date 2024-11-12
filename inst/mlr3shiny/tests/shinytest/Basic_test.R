app <- ShinyDriver$new("../../", seed = 42, loadTimeout = 90000)
app$snapshotInit("basic_test")

app$snapshot()
