context("general AFM test")

test_that("check file does not exist", {
  filename = 'doesNOTexistCheck.tiff'
  d = AFM.read(filename)
  expect_equal(d, NULL)
})


test_that("check info on PARK files", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='tiff$', recursive = TRUE)[1])
  d = AFM.info(filename)
  expect_true(d["INFO.type"]=='Park')
  expect_equal(length(grep('^INFO',names(d))), 7)    # all mandatory fields are set
  expect_equal(d["INFO.widthPixel"],256)
})



test_that("check info on Asylum Research files", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='ibw$', recursive = TRUE)[1])
  d = AFM.info(filename)
  expect_true(d["INFO.type"]=='AR')
  expect_equal(length(grep('^INFO',names(d))), 7)    # all mandatory fields are set
  expect_equal(d["INFO.widthPixel"],256)
})


test_that("flattening image", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='tiff$', recursive = TRUE)[1])
  d = AFM.read(filename)
  expect_equal(length(AFM.flatten(d)),256*256)
})
