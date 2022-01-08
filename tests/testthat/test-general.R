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

test_that("line Profile", {
  filename = AFM.getSampleImages(type='ibw')
  d = AFM.import(filename)
  AFM.lineProfile(d, 0,0, 2000,2000) -> d1
  AFM.lineProfile(d1, 0,0, 100,2500) -> d2
  q = AFM.linePlot(d2,dataOnly=TRUE)
  expect_equal(sum(q$y), 128.764, tolerance = 1e-4)
  expect_equal(nlevels(as.factor(q$type)), 2)
})
