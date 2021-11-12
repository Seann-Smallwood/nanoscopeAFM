context("Park AFM image check")

test_that("check Park AFM images is read into memory", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='tiff$', recursive = TRUE)[1])
  d = read.Park_file(filename)
  expect_equal(nrow(d),65536)
  expect_equal(min(d$z), -0.002049975)
  expect_equal(max(d$x.nm), 2500)
})

test_that("test Park AFM image scales", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='tiff$', recursive = TRUE)[1])
  d = AFM.read(filename)
  expect_equal(length(d), 6)
  expect_equal(nrow(d),256*256)
})

