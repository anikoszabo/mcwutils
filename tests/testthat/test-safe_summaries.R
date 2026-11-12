test_that("safe_max / safe_min", {
  expect_equal(safe_max(c(1, NA, 3)), 3)
  expect_identical(safe_max(c(NA_real_, NA_real_)), NA_real_)
  expect_identical(safe_min(NA_integer_), NA_integer_)
  expect_equal(safe_min(as.Date(c(NA, "2024-03-01", "2024-01-15"))), as.Date("2024-01-15"))
  expect_s3_class(safe_max(as.Date(c(NA, NA))), "Date")
})
test_that("first_non_na", {
  expect_equal(first_non_na(c(NA, NA, "b", "c")), "b")
  expect_equal(first_non_na(c(2, 3)), 2)
  expect_identical(first_non_na(c(NA_character_, NA_character_)), NA_character_)
})
