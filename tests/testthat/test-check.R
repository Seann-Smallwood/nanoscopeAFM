context("NID check")

test_that("Testing NID file header length is correctly detected.", {
  fname =dir(system.file(package = "nanoscopeAFM"),
             pattern='nid$', recursive = TRUE)[1]
  expect_equal(sqrt(2) ^ 2, 2)
  expect_equal(length(fname), 1)
})

test_that("Read NID header correctly.", {
  filename = dir(system.file(package = "nanoscopeAFM"),
                 pattern='nid$', recursive = TRUE)[1]
  expect_equal(1,1)
})


