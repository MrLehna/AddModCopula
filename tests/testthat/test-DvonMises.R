test_that("Testing of distribution", {
  expect_equal(DvonmisesRad(1,1,1),suppressWarnings(circular::dvonmises(1,1,1)))
})
