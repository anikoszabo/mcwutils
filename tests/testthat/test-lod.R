test_that("lod_to_numeric", {
  expect_equal(lod_to_numeric(c("<0.5", "< 0.5", "<.5", "1.2", NA, ".5")),
               c(0.25, 0.25, 0.25, 1.2, NA, 0.5))
  expect_equal(lod_to_numeric("<2", fraction = 1), 2)
  expect_equal(lod_to_numeric(c(1, 2)), c(1, 2))
})
