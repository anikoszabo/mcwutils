test_that("set_label preserves attributes", {
  x <- structure(1:3, foo = "bar")
  y <- set_label(x, "L")
  expect_equal(attr(y, "label"), "L")
  expect_equal(attr(y, "foo"), "bar")
})
test_that("set_label_tibble", {
  d <- set_label_tibble(data.frame(a = 1:2), a, "A var")
  expect_equal(attr(d$a, "label"), "A var")
})
