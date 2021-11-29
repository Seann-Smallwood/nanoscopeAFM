context("Importing AFM file")

# == Asylum Research Igor image

test_that("importing Asylum Research AR AFM sample file", {
  filename = system.file("extdata", "AR_20211011.ibw",package="nanoscopeAFM")
  d = AFM.import(filename)
  expect_true(AFM.isImage(d))
})

test_that("Asylum Research Igor AFM image size check", {
  filename = system.file("extdata", "AR_20211011.ibw",package="nanoscopeAFM")
  d = AFM.import(filename)
  # image is 4 x 4 um
  df = AFM.raster(d)
  expect_equal(max(df$x),4000)
  expect_equal(max(df$y),4000)
  expect_equal(d@y.pixels,128)
})

test_that("Asylum Research Igor AFM image size check", {
  filename = system.file("extdata", "AR_20211011.ibw",package="nanoscopeAFM")
  d = AFM.import(filename)
  r = summary(d)
  # min height -32.46nm
  # max height 50.79nm
  expect_equal(r$z.min.nm,-32.46,tolerance = 1e-2)
  expect_equal(r$z.max.nm,50.79,tolerance = 1e-2)

})

# == Park

test_that("importing Park AFM sample file", {
  filename = system.file("extdata", "Park_20210916_034.tiff",package="nanoscopeAFM")
  d = AFM.import(filename)
  expect_true(AFM.isImage(d))
})

test_that("Park AFM image size check", {
  filename = system.file("extdata", "Park_20210916_034.tiff",package="nanoscopeAFM")
  d = AFM.import(filename)
  # image is 4 x 4 um
  df = AFM.raster(d)
  expect_equal(max(df$x),2500)
  expect_equal(max(df$y),2500)
  expect_equal(d@y.pixels,256)
})

test_that("Park AFM image size check", {
  filename = system.file("extdata", "Park_20210916_034.tiff",package="nanoscopeAFM")
  d = AFM.import(filename)
  r = summary(d)
  # min height -2.71nm
  # max height 9.24nm
  expect_equal(r$z.min.nm,-2.71, tolerance=1e-2)
  expect_equal(r$z.max.nm,9.24, tolerance=1e-3)
})


# == Veeco

test_that("importing Veeco AFM sample file", {
  filename = system.file("extdata", "Veeco_20160622.003",package="nanoscopeAFM")
  d = AFM.import(filename)
  expect_true(AFM.isImage(d))
})


test_that("Veeco AFM image size check", {
  filename = system.file("extdata", "Veeco_20160622.003",package="nanoscopeAFM")
  d = AFM.import(filename)
  # image is 1.7 x 1.7 um
  df = AFM.raster(d)
  expect_equal(max(df$x),1769,tolerance = 1e-4)
  expect_equal(max(df$y),1769,tolerance = 1e-4)
  expect_equal(d@y.pixels,512)
})

test_that("Veeco AFM image size check", {
  filename = system.file("extdata", "Veeco_20160622.003",package="nanoscopeAFM")
  d = AFM.import(filename)
  r = summary(d)
  # min height -145.6nm
  # max height 13.9nm
  expect_equal(r$z.min.nm,-145.6,tolerance = 1e-2)
  expect_equal(r$z.max.nm,13.9,tolerance = 1e-2)
})


# == NanoSurf

test_that("importing NanoSurf AFM sample file", {
  filename = system.file("extdata", "NanoSurf_20160301.nid",package="nanoscopeAFM")
  d = AFM.import(filename)
  expect_true(AFM.isImage(d))
})

test_that("NanoSurf image size check", {
  filename = system.file("extdata", "NanoSurf_20160301.nid",package="nanoscopeAFM")
  d = AFM.import(filename)
  # image is 10 x 10 um
  expect_equal(d@x.pixels*d@x.conv,10000)
  expect_equal(d@y.pixels*d@y.conv,10000)
})

test_that("NanoSurf image size check", {
  filename = system.file("extdata", "NanoSurf_20160301.nid",package="nanoscopeAFM")
  d = AFM.import(filename)
  r = summary(d)
  # min height -294.2nm
  # max height -88.5nm
  expect_equal(r$z.min.nm,-294.2,tolerance = 1e-2)
  expect_equal(r$z.max.nm,-88.5,tolerance = 1e-2)
})


