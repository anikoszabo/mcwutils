test_that("paste_na", {
  expect_equal(paste_na(c("a", NA, NA), c("b", "c", NA)), c("a b", "c", NA))
  expect_equal(paste_na(c("a", NA), c("b", "c"), c(NA, "d"), sep = "-"), c("a-b", "c-d"))
  expect_error(paste_na(1:2, 1:3))
})
test_that("str_capitalize_first", {
  expect_equal(str_capitalize_first(c("hELLO", NA)), c("HELLO", NA))
})
test_that("str_ascii", {
  expect_equal(str_ascii("caf\u00e9"), "cafe")
  expect_equal(str_ascii("caf\u00e9", transliterate = FALSE), "caf")
})
