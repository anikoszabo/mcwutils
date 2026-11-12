test_that("source_skip skips lines", {
  f <- tempfile(fileext = ".R")
  writeLines(c("stop('should be skipped')", "src_skip_test_val <- 7"), f)
  source_skip(f, skip = 1, local = FALSE)
  expect_equal(get("src_skip_test_val", envir = globalenv()), 7)
  rm("src_skip_test_val", envir = globalenv())
})
test_that("source_skip restores working directory", {
  f <- tempfile(fileext = ".R"); writeLines("1", f)
  wd <- getwd(); source_skip(f, chdir = TRUE)
  expect_equal(getwd(), wd)
})
test_that("source_rmd runs chunks", {
  f <- tempfile(fileext = ".Rmd")
  writeLines(c("```{r}", "src_rmd_test_val <- 3", "```"), f)
  source_rmd(f)
  expect_equal(get("src_rmd_test_val", envir = globalenv()), 3)
  rm("src_rmd_test_val", envir = globalenv())
})
