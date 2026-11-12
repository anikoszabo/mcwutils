test_that("dict_parse_choices handles either side and code 0", {
  expect_equal(dict_parse_choices(c("1 = Male", "2 = Female")), c(`1` = "Male", `2` = "Female"))
  expect_equal(dict_parse_choices(c("Male = 1", "Female = 0")), c(`1` = "Male", `0` = "Female"))
  expect_equal(dict_parse_choices(c("a = b"), default = 2), c(b = "a"))
  expect_length(dict_parse_choices("no equals"), 0)
})
test_that("dict_recode_var", {
  d <- data.frame(sex = c(1, 2, 1), age = 1:3)
  g <- list(sex = c("1 = Male", "2 = Female"))
  expect_equal(as.character(dict_recode_var("sex", d, g)), c("Male", "Female", "Male"))
  expect_equal(dict_recode_var("age", d, g), d$age)
})
test_that("copy_factor_labels", {
  d <- data.frame(sex = 1:2, sex.factor = factor(c("M", "F")), orphan.factor = factor("a")[c(1, 1)])
  attr(d$sex, "label") <- "Sex"
  r <- copy_factor_labels(d)
  expect_equal(attr(r$sex.factor, "label"), "Sex")
  expect_null(attr(r$orphan.factor, "label"))
})
