#' Paste vectors, ignoring missing values
#'
#' Like [paste()], but `NA` elements are dropped instead of printed as `"NA"`.
#' Where all elements are `NA`, the result is `NA`.
#'
#' @param x,y,... Vectors of equal length (coerced to character).
#' @param sep Separator.
#' @return Character vector of the common length.
#' @export
#' @examples
#' paste_na(c("a", NA, NA), c("b", "c", NA))
#' paste_na(c("a", NA), c("b", "c"), c(NA, "d"), sep = "; ")
paste_na <- function(x, y, ..., sep = " ") {
  vecs <- list(x, y, ...)
  if (length(unique(lengths(vecs))) != 1L) stop("All inputs must have the same length.", call. = FALSE)
  mat <- do.call(cbind, lapply(vecs, as.character))
  out <- apply(mat, 1L, function(r) {
    r <- r[!is.na(r)]
    if (length(r) == 0L) NA_character_ else paste(r, collapse = sep)
  })
  as.character(out)
}

#' Capitalize the first character of each string
#'
#' Upper-cases the first word character and leaves the rest unchanged (unlike
#' [stringr::str_to_sentence()], which lower-cases the remainder).
#'
#' @param x Character vector.
#' @return Character vector.
#' @export
#' @examples
#' str_capitalize_first(c("hELLO wORLD", "abc", NA))
str_capitalize_first <- function(x) {
  stringr::str_replace(x, "^\\w{1}", toupper)
}

#' Convert strings to ASCII
#'
#' Either transliterates accented characters to their closest ASCII equivalents
#' or deletes all non-ASCII characters. To locate non-ASCII characters first,
#' see [tools::showNonASCII()].
#'
#' @param x Character vector.
#' @param transliterate If `TRUE` (default) use [stringi::stri_trans_general()]
#'   with `"Latin-ASCII"`; if `FALSE` remove non-ASCII characters.
#' @return Character vector.
#' @export
#' @examples
#' x <- c("caf\u00e9", "na\u00efve", "M\u00fcller")
#' str_ascii(x)
#' str_ascii(x, transliterate = FALSE)
str_ascii <- function(x, transliterate = TRUE) {
  if (transliterate) {
    stringi::stri_trans_general(x, "Latin-ASCII")
  } else {
    stringr::str_remove_all(x, "[^\\u0001-\\u007F]+")
  }
}
