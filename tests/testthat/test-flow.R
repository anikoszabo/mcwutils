d <- data.frame(id = c(1, 1, 2, 3, 4), age = c(50, 50, 17, 62, NA))

test_that("exclude: TRUE rows are discarded, NA kept", {
  fl <- start_flow(d, "All", idvar = "id")
  expect_equal(fl$flow$`# Remaining`, 4)
  fl <- add_to_flow(fl, "Adults", exclude = age < 18)
  expect_equal(fl$flow$`# Remaining`[2], 3)
  expect_equal(fl$flow$`# Excluded`[2], 1)
  expect_equal(fl$flow$`Excluded IDs`[2], "2")
  expect_equal(fl$flow$`Criterion type`[2], "exclude")
  expect_equal(fl$flow$Criterion[2], "age < 18")
  expect_equal(nrow(fl$data), 4)
})

test_that("include: TRUE rows kept, NA discarded", {
  fl <- start_flow(d, "All", idvar = "id") |> add_to_flow("Adults", include = age >= 18)
  expect_equal(fl$flow$`# Remaining`[2], 2)
  expect_equal(fl$flow$`Excluded IDs`[2], "2, 4")
  expect_equal(fl$flow$`Criterion type`[2], "include")
  expect_equal(nrow(fl$data), 3)
})

test_that("exactly one of exclude / include", {
  fl <- start_flow(d, "All")
  expect_error(add_to_flow(fl, "x"))
  expect_error(add_to_flow(fl, "x", exclude = age < 18, include = age >= 18))
})

test_that("flow without idvar counts rows", {
  fl <- d |> start_flow("All") |> add_to_flow("Adults", include = age >= 18)
  expect_equal(fl$flow$`# Remaining`, c(5, 3))
  expect_true(is.na(fl$flow$`Excluded IDs`[2]))
})

test_that("zero and all exclusions", {
  fl <- start_flow(d, "All", idvar = "id")
  none <- add_to_flow(fl, "none", exclude = FALSE)
  expect_equal(none$flow$`# Excluded`[2], 0)
  expect_true(is.na(none$flow$`Excluded IDs`[2]))
  all <- add_to_flow(fl, "all", exclude = TRUE)
  expect_equal(all$flow$`# Remaining`[2], 0)
  expect_equal(all$flow$`Excluded IDs`[2], "1, 2, 3, 4")
})

test_that("bad idvar errors", {
  expect_error(start_flow(d, "All", idvar = "zzz"))
})

test_that("show_flow hides criterion columns and optionally IDs", {
  skip_if_not_installed("gt")
  fl <- start_flow(d, "All", idvar = "id") |> add_to_flow("Adults", include = age >= 18)
  cols <- function(x) names(x[["_data"]])
  expect_equal(cols(show_flow(fl)), c("Patient set", "# Remaining", "# Excluded", "Excluded IDs"))
  expect_false("Excluded IDs" %in% cols(show_flow(fl, show_excluded_IDs = FALSE)))
  fl0 <- start_flow(d, "All") |> add_to_flow("Adults", include = age >= 18)
  expect_false("Excluded IDs" %in% cols(show_flow(fl0)))
  expect_true("Excluded IDs" %in% cols(show_flow(fl0, show_excluded_IDs = TRUE)))
})
