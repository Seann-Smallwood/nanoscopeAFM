context("Asylum Research AFM image check")

filename = system.file("extdata", "AR_20211011.ibw",package="nanoscopeAFM")

test_that("AR: filefound", {
  expect_true(file.exists(filename))
})

test_that("AR: check version 2 channel list ", {
  h1 = read.AR_eofHeader.V2(filename)
  expect_equal(length(h1), 13)
})


test_that("AR: check image roughness ", {
  d = AFMmath.params(AFM.import(filename))
  expect_equal(d$Ra, 6.365067, tolerance = 1e-5)
  expect_equal(d$Rq, 7.865308, tolerance = 1e-5)
})
