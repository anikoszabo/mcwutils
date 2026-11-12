#' Convert laboratory values reported as below the limit of detection
#'
#' Converts character values such as `"<0.5"` to numbers by replacing the
#' censored value with a fraction of the detection limit (default one half).
#' Other numeric strings are converted as is. Whitespace is tolerated.
#'
#' @param x Character (or numeric) vector.
#' @param fraction Multiplier applied to the limit `L` for values of the form `<L`.
#' @return Numeric vector; unparseable values become `NA` with a warning.
#' @export
#' @examples
#' lod_to_numeric(c("<0.5", "1.2", "< 2", NA, ".5"))
#' lod_to_numeric(c("<0.5", "1.2"), fraction = 1 / sqrt(2))
lod_to_numeric <- function(x, fraction = 0.5) {
  stopifnot(is.numeric(fraction), length(fraction) == 1L)
  if (is.numeric(x)) return(as.numeric(x))
  x <- stringr::str_trim(as.character(x))
  below <- !is.na(x) & stringr::str_detect(x, "^<")
  out <- as.numeric(stringr::str_trim(stringr::str_remove(x, "^<")))
  out[below] <- out[below] * fraction
  out
}
