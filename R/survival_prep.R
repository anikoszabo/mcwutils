#' Administratively censor time-to-event data
#'
#' Observations with follow-up beyond `at` are censored at `at`: the time is set
#' to `at` and the event indicator(s) to 0.
#'
#' @param data A data frame.
#' @param time_var Name (string) of the follow-up time column.
#' @param ind_vars Character vector of event-indicator column names.
#' @param at Censoring time, in the units of `time_var`.
#' @return `data` with modified `time_var` and `ind_vars`.
#' @export
#' @examples
#' d <- data.frame(time = c(2, 5, 9), death = c(1, 0, 1), relapse = c(0, 1, 1))
#' admin_censor(d, "time", c("death", "relapse"), at = 6)
admin_censor <- function(data, time_var, ind_vars, at) {
  stopifnot(is.data.frame(data), is.character(time_var), length(time_var) == 1L,
            is.character(ind_vars), is.numeric(at), length(at) == 1L)
  miss <- setdiff(c(time_var, ind_vars), names(data))
  if (length(miss)) stop("Column(s) not found in `data`: ", paste(miss, collapse = ", "), call. = FALSE)
  over <- !is.na(data[[time_var]]) & data[[time_var]] > at
  data[[time_var]][over] <- at
  for (v in ind_vars) data[[v]][over] <- 0
  data
}
