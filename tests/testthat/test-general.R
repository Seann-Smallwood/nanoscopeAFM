context("general AFM test")

test_that("check loading non-existant file", {
  filename = 'doesNOTexistCheck.tiff'
  d = AFM.read(filename)
  expect_equal(nrow(d), 0)
})

test_that("AFMdata object", {
  a = AFMdata(instrument='AR')
  expect_true(isS4(a))
})

test_that("flattening image", {
  filename = system.file("extdata", "NanoSurf_20160301.nid",package="nanoscopeAFM")
  d = AFM.import(filename)
  d2 = AFM.flatten(d)
  expect_equal(nrow(d@data),nrow(d2@data))
})



context("AFM info header information")

COMMON.INFO.ITEMS = 9

test_that("PARK AFM info", {
  filename = system.file("extdata", "Park_20210916_034.tiff",package="nanoscopeAFM")
  d = AFM.info(filename)
  expect_equal(length(grep('^INFO',names(d))), COMMON.INFO.ITEMS)    # all mandatory fields are set
  expect_equal(as.numeric(d["INFO.widthPixel"]),256)
})

test_that("NanoSurf AFM info", {
  filename = system.file("extdata", "NanoSurf_20160301.nid",package="nanoscopeAFM")
  d = AFM.info(filename)
  expect_equal(length(grep('^INFO',names(d))), COMMON.INFO.ITEMS)    # all mandatory fields are set
  expect_equal(as.numeric(d["INFO.widthPixel"]),128)
})

test_that("Asylum Research AFM info", {
  filename = system.file("extdata", "AR_20211011.ibw",package="nanoscopeAFM")
  d = AFM.info(filename)
  expect_equal(length(grep('^INFO',names(d))), COMMON.INFO.ITEMS)    # all mandatory fields are set
  # dput(d["INFO.widthPixel"])
  expect_equal(as.numeric(d["INFO.widthPixel"]),128)
})

# test_that("Veeco AFM info", {
#   filename = system.file("extdata", "Veeco_20160622.003",package="nanoscopeAFM")
#   d = AFM.info(filename)
#   expect_equal(length(grep('^INFO',names(d))), COMMON.INFO.ITEMS)    # all mandatory fields are set
#   expect_equal(as.numeric(d["INFO.widthPixel"]),512)
# })




