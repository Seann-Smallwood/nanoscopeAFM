context("general AFM test")


test_that("AFMdata object", {
  a = AFMdata(instrument='Cypher')
  expect_true(isS4(a))
})

test_that("AFM.flatten: flattening image", {
  filename = system.file("extdata", "NanoSurf_20160301.nid",package="nanoscopeAFM")
  d = AFM.import(filename)
  d2 = AFM.flatten(d)
  expect_equal(nrow(d@data),nrow(d2@data))
})

test_that("AFM.getSampleImages: find sample files", {
  file.list = AFM.getSampleImages()
  expect_equal(length(file.list),4)
  expect_true(file.exists(file.list[1]))
})

