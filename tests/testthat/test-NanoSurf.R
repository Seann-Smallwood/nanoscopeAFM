context("NanoSurf AFM image check")

test_that("check NID file reads correctly", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='nid$', recursive = TRUE)[1])
  d = NID.checkFile(filename)
  expect_equal(d,0)
})


test_that("read NID image and check some data", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='nid$', recursive = TRUE)[1])
  d = NID.loadImage(filename,1)
  expect_equal(nrow(d),128*128)
  expect_equal(max(d$z), -1.00885e-05)
  expect_equal(max(d$x), 1e-5)
})


test_that("use general AFM reading function to read NanoSurf file", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='nid$', recursive = TRUE)[1])
  d = AFM.read(filename)
  expect_equal(length(d), 6)
  expect_equal(nrow(d),128*128)
  expect_equal(max(d$z),-290)
})
