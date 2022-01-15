context("general AFM test")

filename = AFM.getSampleImages(type='ibw')
afmd = AFM.import(filename)



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
  AFM.lineProfile(afmd, 0,0, 2000,2000) -> d1
  AFM.lineProfile(d1, 0,0, 100,2500) -> d2
  q = AFM.linePlot(d2, dataOnly=TRUE)
  expect_equal(sum(q$z), -103.9477, tolerance = 1e-4)
  expect_equal(nlevels(as.factor(q$type)), 2)
})

test_that("print AFMdata", {
  expect_output(print(afmd), "Cypher AFM Image")
  expect_output(print(afmd), "HeightRetrace")
})


test_that("summary AFMinfo", {
  h = AFMinfo(AFM.getSampleImages(type='ibw')[1])
  expect_output(summary(h), "Scan Angle")
})



test_that("Graphing ggplot2 AFM image", {
  p = plot(afmd)
  expect_equal(class(p$layers[[1]]$geom), c("GeomRaster", "Geom","ggproto","gg"))
  p = plot(afmd, graphType = 2)
  expect_equal(class(p$layers[[1]]$geom), c("GeomRaster", "Geom","ggproto","gg"))
  p = plot(afmd, graphType = 3)
  expect_equal(class(p$layers[[1]]$geom), c("GeomRaster", "Geom","ggproto","gg"))
})




test_that("test histogram for image and data", {
  expect_equal( sum(AFM.histogram(afmd, dataOnly=TRUE)$zDensity), 1)
  p = AFM.histogram(afmd)
  expect_equal(class(p$layers[[1]]$geom), c("GeomBar", "GeomRect", "Geom","ggproto","gg"))
})



test_that("valid AFM files", {
  file.list = AFM.getSampleImages()
  for(filename in file.list) {
    expect_true(AFM.isFileValid(filename))
  }
  expect_true(!AFM.isFileValid("random-nonexistant-file.txt"))
})

