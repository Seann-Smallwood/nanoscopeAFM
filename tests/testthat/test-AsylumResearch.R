context("Asylum Research AFM image check")

COMMON.INFO.ITEMS = 7


test_that("AR: check channels ", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  h1 = read.AR_eofHeader.V2(filename)
  ch = .getChannelName(h1,1)
  expect_equal(.getChannelUnits(ch),"m")
  ch = .getChannelName(h1,2)
  expect_equal(.getChannelUnits(ch),"m")
  ch = .getChannelName(h1,3)
  expect_equal(.getChannelUnits(ch),"deg")
  ch = .getChannelName(h1,4)
  expect_equal(.getChannelUnits(ch),"m")
  expect_equal(as.numeric(h1$NumberOfFiles),4)
})


test_that("check AR image loads", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  d = read.AR_file(filename,1)
  expect_equal(attr(d,'units'),"m")
  expect_equal(nrow(d),128*128)
  expect_equal(min(d$x.nm), 0)
  expect_equal(min(d$y.nm), 0)
  expect_equal(max(d$x.nm), 4000)
  expect_equal(max(d$y.nm), 4000)
})


test_that("check AR image loads for image 2", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  d = read.AR_file(filename,2)
  expect_equal(attr(d,'units'),"m")
  expect_equal(nrow(d),128*128)

  expect_equal(min(d$x.nm), 0)
  expect_equal(min(d$y.nm), 0)
  expect_equal(max(d$x.nm), 4000)
  expect_equal(max(d$y.nm), 4000)
})


test_that("check AR image loads for image 3", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  d = read.AR_file(filename,3)
  expect_equal(attr(d,'units'),"deg")
  expect_equal(nrow(d),128*128)

  expect_equal(min(d$x.nm), 0)
  expect_equal(min(d$y.nm), 0)
  expect_equal(max(d$x.nm), 4000)
  expect_equal(max(d$y.nm), 4000)
})


test_that("check AR image loads for image 4", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  d = read.AR_file(filename,4)
  expect_equal(attr(d,'units'),"m")
  expect_equal(nrow(d),128*128)

  expect_equal(min(d$x.nm), 0)
  expect_equal(min(d$y.nm), 0)
  expect_equal(max(d$x.nm), 4000)
  expect_equal(max(d$y.nm), 4000)
})


test_that("use general AFM loading for AR image", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  d = AFM.read(filename)
  expect_equal(nrow(d),128*128)
})



test_that("AR: check version 2 channel list ", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  h1 = read.AR_eofHeader.V2(filename)
  expect_equal(length(h1), 13)
})

