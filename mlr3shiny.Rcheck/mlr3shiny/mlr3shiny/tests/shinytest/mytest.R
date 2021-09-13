app <- ShinyDriver$new("../../", seed = 42, loadTimeout = 1e+07)
app$snapshotInit("mytest")

app$snapshot()
