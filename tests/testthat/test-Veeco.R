context("Veeco AFM image check")

test_that("check if Veeco AFM file reads", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='\\d{3}$', recursive = TRUE)[1])
  d = read.Nanoscope_file(filename)
  expect_equal(nrow(d),512*512)
})



test_that("use general AFM reading function to read file", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='\\d{3}$', recursive = TRUE)[1])
  d = AFM.read(filename)
  expect_equal(length(d), 6)
  expect_equal(nrow(d),512*512)
  expect_equal(max(d$z),3121)
})
