context("NanoSurf AFM image check")

test_that("check NID file reads correctly", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='nid$', recursive = TRUE)[1])
  d = NID.checkFile(filename)
  expect_equal(d,0)
})


test_that("use general AFM reading function to read NanoSurf file", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='nid$', recursive = TRUE)[1])
  system.file("NanoSurf_20160301.nid",package="nanoscopeAFM")
  d = AFM.read(filename)
  expect_equal(length(d), 6)
  expect_equal(nrow(d),128*128)
  expect_equal(max(d$z),-290)
  expect_equal(max(d$x), 128)
  mean(d$z.nm)
})


# test_that("NanoSurf z calibration", {
#   filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
#                                         pattern='nid$', recursive = TRUE)[1])
#   d = AFM.read(filename)
#   expect_equal(max(d$z.nm), -88.5)  # from Gwyddion
#   expect_equal(min(d$z.nm), -294.2)
# })

