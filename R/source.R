#' Source a file, skipping its first lines
#'
#' Reads a script, drops the first `skip` lines and evaluates the rest. Useful
#' for REDCap export scripts whose first lines clear the workspace or load
#' packages. Optionally evaluates with the file's directory as working directory.
#'
#' @param file Path to an R script.
#' @param skip Number of initial lines to skip.
#' @param chdir If `TRUE`, temporarily set the working directory to `dirname(file)`.
#' @param ... Passed to [base::source()].
#' @return Invisibly, the value returned by [base::source()].
#' @export
#' @examples
#' f <- tempfile(fileext = ".R")
#' writeLines(c("rm(list = ls())", "y_skip_demo <- 42"), f)
#' source_skip(f, skip = 1)
#' y_skip_demo
source_skip <- function(file, skip = 0, chdir = FALSE, ...) {
  lines <- scan(file, what = character(), skip = skip, sep = "\n", quiet = TRUE)
  if (chdir) {
    old <- setwd(dirname(normalizePath(file)))
    on.exit(setwd(old), add = TRUE)
  }
  invisible(source(textConnection(paste(lines, collapse = "\n")), ...))
}

#' Source the R code of an R Markdown file
#'
#' Extracts the code chunks with [knitr::purl()] into a temporary script and
#' sources it.
#'
#' @param file Path to an `.Rmd` file.
#' @param ... Passed to [base::source()].
#' @return Invisibly, the value returned by [base::source()].
#' @export
source_rmd <- function(file, ...) {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  knitr::purl(file, output = tmp, quiet = TRUE)
  invisible(source(tmp, ...))
}
