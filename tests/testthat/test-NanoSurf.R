context("NanoSurf AFM image check")


filename = system.file("extdata", "NanoSurf_20160301.nid",package="nanoscopeAFM")


test_that("check NID file reads correctly", {
  d = NID.checkFile(filename)
  expect_equal(d,0)
})


test_that("use general AFM reading function to read NanoSurf file", {
  d = AFM.read(filename)
  expect_equal(length(d), 6)
  expect_equal(nrow(d),128*128)
  expect_equal(max(d$z),-290)
  expect_equal(max(d$x), 128)
  mean(d$z.nm)
})

