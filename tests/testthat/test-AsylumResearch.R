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
  expect_equal(nrow(d),128*128)
})


test_that("use general AFM loading for AR image", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  d = AFM.read(filename)
  expect_equal(nrow(d),128*128)
})


test_that("check AR image size ", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  d = AFM.read(filename)
  expect_equal(signif(max(d$x.nm),3), signif(4030,3))
})


# test_that("AR: loading image out of bounds ", {
#   filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
#                                         pattern='ibw$', recursive = TRUE)[1])
#   d = AFM.read(filename,20)
#   expect_equal(length(d), 0)
# })

test_that("AR: check version 2 channel list ", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  h1 = read.AR_eofHeader.V2(filename)
  expect_equal(length(h1), 13)
})

