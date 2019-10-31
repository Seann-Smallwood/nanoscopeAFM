context("NID check")

test_that("check NID file reads correctly", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                                        pattern='nid$', recursive = TRUE)[1])
  d = NID.checkFile(filename)
  expect_equal(d,0)
})

test_that("Testing NID file header length is correctly detected.", {
  fname =dir(system.file(package = "nanoscopeAFM"),
             pattern='nid$', recursive = TRUE)[1]
  expect_equal(length(fname), 1)
})

test_that("Read NID image correctly.", {
  filename = file.path('../../inst',dir(system.file(package = "nanoscopeAFM"),
                 pattern='nid$', recursive = TRUE)[1])
  d = NID.loadImage(filename,1)
  expect_equal(nrow(d),128*128)
})



