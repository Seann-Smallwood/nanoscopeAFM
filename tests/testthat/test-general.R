context("general AFM test")

test_that("check loading non-existant file", {
  filename = 'doesNOTexistCheck.tiff'
  d = AFM.read(filename)
  expect_equal(d, NULL)
})

test_that("flattening image", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='tiff$', recursive = TRUE)[1])
  d = AFM.read(filename)
  expect_equal(length(AFM.flatten(d)),256*256)
})



context("AFM info header information")

COMMON.INFO.ITEMS = 9

test_that("PARK AFM info", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='tiff$', recursive = TRUE)[1])
  d = AFM.info(filename)
  expect_equal(length(grep('^INFO',names(d))), COMMON.INFO.ITEMS)    # all mandatory fields are set
  expect_equal(as.numeric(d["INFO.widthPixel"]),256)
})

test_that("NanoSurf AFM info", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='nid$', recursive = TRUE)[1])
  d = AFM.info(filename)
  expect_equal(length(grep('^INFO',names(d))), COMMON.INFO.ITEMS)    # all mandatory fields are set
  expect_equal(as.numeric(d["INFO.widthPixel"]),128)
})

test_that("Asylum Research AFM info", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  d = AFM.info(filename)
  expect_equal(length(grep('^INFO',names(d))), COMMON.INFO.ITEMS)    # all mandatory fields are set
  # dput(d["INFO.widthPixel"])
  expect_equal(as.numeric(d["INFO.widthPixel"]),128)
})

test_that("Veeco AFM info", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='\\d$', recursive = TRUE)[1])
  d = AFM.info(filename)
  expect_equal(length(grep('^INFO',names(d))), COMMON.INFO.ITEMS)    # all mandatory fields are set
  expect_equal(as.numeric(d["INFO.widthPixel"]),512)
})


