context("general AFM test")

test_that("check loading non-existant file", {
  filename = 'doesNOTexistCheck.tiff'
  d = AFM.read(filename)
  expect_equal(nrow(d), 0)
})

test_that("AFMdata object", {
  a = AFMdata(instrument='Cypher')
  expect_true(isS4(a))
})

test_that("flattening image", {
  filename = system.file("extdata", "NanoSurf_20160301.nid",package="nanoscopeAFM")
  d = AFM.import(filename)
  d2 = AFM.flatten(d)
  expect_equal(nrow(d@data),nrow(d2@data))
})



