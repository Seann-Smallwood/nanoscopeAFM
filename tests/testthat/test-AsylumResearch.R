context("Asylum Research AFM image check")

test_that("check AR image loads", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  d = read.IBW_file(filename,1)
  expect_equal(nrow(d),128*128)
})




test_that("use general AFM loading for AR image", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  d = AFM.read(filename)
  expect_equal(nrow(d),128*128)
  expect_equal(length(d), 6)
})
