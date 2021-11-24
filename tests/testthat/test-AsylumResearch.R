context("Asylum Research AFM image check")

COMMON.INFO.ITEMS = 7

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


test_that("AR: loading image out of bounds ", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  d = AFM.read(filename,20)
  expect_equal(length(d), 0)
})


test_that("AR: test channel names ", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  s1 = read.AR_channelNames(filename)
  #dput(s1)
  expect_equal(length(s1), 4)
})
