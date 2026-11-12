#' Maximum, minimum and first non-missing value that tolerate all-`NA` input
#'
#' `safe_max()` and `safe_min()` ignore `NA`s like `max(x, na.rm = TRUE)` but
#' return a typed `NA` instead of `-Inf`/`Inf` (with a warning) when all values
#' are missing. `first_non_na()` returns the first non-missing value, or `NA`.
#' Useful inside [dplyr::summarise()].
#'
#' @param x A vector (numeric, integer, Date; character for `first_non_na()`).
#' @return Length-1 vector of the same type as `x`.
#' @name safe_summaries
#' @examples
#' safe_max(c(1, NA, 3))
#' safe_max(c(NA_real_, NA_real_))
#' safe_min(as.Date(c(NA, "2024-03-01", "2024-01-15")))
#' first_non_na(c(NA, NA, "b", "c"))
#' first_non_na(c(NA_character_, NA_character_))
NULL

#' @rdname safe_summaries
#' @export
safe_max <- function(x) {
  if (all(is.na(x))) return(x[NA_integer_][1L])
  max(x, na.rm = TRUE)
}

#' @rdname safe_summaries
#' @export
safe_min <- function(x) {
  if (all(is.na(x))) return(x[NA_integer_][1L])
  min(x, na.rm = TRUE)
}

#' @rdname safe_summaries
#' @export
first_non_na <- function(x) {
  ok <- !is.na(x)
  if (!any(ok)) return(x[NA_integer_][1L])
  x[ok][1L]
}
