context("Park AFM image check")

filename = system.file("extdata", "Park_20210916_034.tiff",package="nanoscopeAFM")


test_that("check Park AFM images is read into memory", {
  d = read.Park_file(filename)
  expect_equal(nrow(d),65536)
  expect_equal(min(d$z), -0.002049975)
  expect_equal(max(d$x.nm), 2500)
})

test_that("test Park AFM image scales", {
  d = AFM.read(filename)
  expect_equal(length(d), 6)
  expect_equal(nrow(d),256*256)
})

test_that("Park image import", {
  afmobj = AFM.import(filename)
  expect_equal(afmobj@x.pixels, 256)
})


test_that("Park AFM image roughness check", {
  d = AFM.math.params(AFM.import(filename))
  expect_equal(d$Ra, 0.3788, tolerance = 1e-4)
  expect_equal(d$Rq, 0.6351, tolerance = 1e-4)
})
