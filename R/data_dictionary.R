#' Parse REDCap-style choice strings
#'
#' Parses a vector of choice entries of the form `"code = label"` (one entry per
#' element; entries without `=` are dropped). In each entry the side that is an
#' integer in 0-100 is taken as the code; if neither side qualifies, side
#' `default` is the code.
#'
#' @param choices Character vector of `"code = label"` strings.
#' @param default Which side (1 or 2) is the code when it cannot be determined.
#' @return Named character vector of labels, named by code.
#' @export
#' @examples
#' dict_parse_choices(c("1 = Male", "2 = Female"))
#' dict_parse_choices(c("Male = 1", "Female = 2"))
dict_parse_choices <- function(choices, default = 1) {
  stopifnot(default %in% 1:2)
  choices <- stringr::str_subset(choices, stringr::fixed("="))
  if (length(choices) == 0L) return(stats::setNames(character(0), character(0)))
  tab <- stringr::str_split_fixed(choices, "=", 2)
  tab <- apply(tab, 2, stringr::str_trim)
  if (is.null(dim(tab))) tab <- matrix(tab, nrow = 1)
  code_idx <- apply(tab, 1, function(x) {
    i <- which(x %in% as.character(0:100))
    if (length(i) == 1L) i else default
  })
  idx <- cbind(seq_len(nrow(tab)), code_idx)
  codes <- tab[idx]
  labs <- tab[cbind(seq_len(nrow(tab)), 3 - code_idx)]
  names(labs) <- codes
  labs
}

#' Recode a variable to a factor using a data dictionary
#'
#' @param varname Name (string) of the variable in `data`.
#' @param data Data frame holding the variable.
#' @param glossary Named list (or data frame) whose element `varname` holds the
#'   choice strings (see [dict_parse_choices()]).
#' @return The variable recoded as a factor with labels, or unchanged if the
#'   glossary has no usable choices for it.
#' @export
#' @examples
#' demo <- data.frame(sex = c(1, 2, 1), age = c(30, 40, 50))
#' glo <- list(sex = c("1 = Male", "2 = Female"))
#' for (var in names(demo)) demo[[var]] <- dict_recode_var(var, demo, glo)
#' demo
dict_recode_var <- function(varname, data, glossary) {
  x <- data[[varname]]
  ch <- glossary[[varname]]
  if (is.null(ch) || all(is.na(ch))) return(x)
  codes <- dict_parse_choices(ch)
  if (length(codes) == 0L) return(x)
  factor(x, levels = names(codes), labels = codes)
}

#' Copy variable labels from base columns to their ".factor" versions
#'
#' In REDCap exports, factor versions of variables have names ending in a suffix
#' (default `".factor"`). This copies the `"label"` attribute from the base
#' column to the suffixed column. Columns whose base column is missing are skipped.
#'
#' @param data A data frame.
#' @param suffix Suffix identifying factor columns.
#' @return `data` with labels set on the suffixed columns.
#' @export
#' @examples
#' d <- data.frame(sex = 1:2, sex.factor = factor(c("M", "F")))
#' attr(d$sex, "label") <- "Sex of participant"
#' attr(copy_factor_labels(d)$sex.factor, "label")
copy_factor_labels <- function(data, suffix = ".factor") {
  for (v in names(data)) {
    if (endsWith(v, suffix)) {
      base <- substr(v, 1L, nchar(v) - nchar(suffix))
      if (base %in% names(data) && !is.null(attr(data[[base]], "label"))) {
        attr(data[[v]], "label") <- attr(data[[base]], "label")
      }
    }
  }
  data
}
