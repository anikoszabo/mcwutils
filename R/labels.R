#' Set a variable label
#'
#' Sets the `"label"` attribute using base R only (no dependency on labelled or
#' summarytools).
#'
#' @param x A vector.
#' @param label Label string.
#' @return `x` with a `"label"` attribute; other attributes are preserved.
#' @export
#' @examples
#' x <- set_label(1:3, "Count")
#' attr(x, "label")
set_label <- function(x, label) {
  attr(x, "label") <- label
  x
}

#' Set a variable label inside a data frame
#'
#' @param tbl A data frame.
#' @param x Unquoted column name.
#' @param label Label string.
#' @return `tbl` with the label set on column `x`.
#' @export
#' @examples
#' d <- data.frame(a = 1:3)
#' d <- set_label_tibble(d, a, "Variable A")
#' attr(d$a, "label")
set_label_tibble <- function(tbl, x, label) {
  dplyr::mutate(tbl, {{ x }} := set_label({{ x }}, label))
}
