#' Track participant exclusions in a selection flow table
#'
#' `start_flow()` begins a flow, `add_to_flow()` applies an exclusion or an
#' inclusion criterion and records how many subjects (and which IDs) were
#' removed, and `show_flow()` formats the result as a table.
#'
#' A flow is a list with elements `data` (currently retained data), `flow` (a
#' tibble) and `idvar`. The tibble has columns `Patient set`, `# Remaining`,
#' `# Excluded`, `Excluded IDs`, plus two bookkeeping columns, `Criterion type`
#' (`"exclude"` or `"include"`) and `Criterion` (the criterion as typed), which
#' are not printed by `show_flow()`.
#'
#' When `idvar` is given, counts are numbers of distinct IDs and excluded IDs
#' are those present before but absent after the step. When `idvar` is `NULL`,
#' rows are counted and `Excluded IDs` is `NA`.
#'
#' @param data A data frame.
#' @param description Label for the starting set or the step (column `Patient set`).
#' @param idvar Optional name (string) of the ID column.
#' @param flow A flow object.
#' @param exclude Unquoted logical expression. Rows where it is `TRUE` are
#'   **discarded**; rows where it is `NA` are kept.
#' @param include Unquoted logical expression. Rows where it is `TRUE` are
#'   **kept**; rows where it is `FALSE` or `NA` are discarded.
#'   Exactly one of `exclude` and `include` must be given.
#' @param title Table title for `show_flow()`.
#' @param show_excluded_IDs Show the `Excluded IDs` column? `NULL` (default)
#'   shows it only if the flow tracks an ID variable.
#' @return A flow object (`start_flow()`, `add_to_flow()`) or a `gt` table
#'   (`show_flow()`; needs the suggested package gt).
#' @name flow
#' @examples
#' d <- data.frame(id = c(1, 1, 2, 3, 4), age = c(50, 50, 17, 62, NA),
#'                 male = c(1, 1, 0, 1, 0))
#' fl <- start_flow(d, "Enrolled in study", idvar = "id")
#' fl <- add_to_flow(fl, "Adults", exclude = age < 18)
#' fl <- add_to_flow(fl, "Male", include = male == 1)
#' fl$flow
#' fl$data
#'
#' # without an ID variable (rows are counted)
#' fl2 <- d |> start_flow("All rows") |> add_to_flow("Adults", include = age >= 18)
#' fl2$flow
#'
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   show_flow(fl)
#' }
NULL

#' @rdname flow
#' @export
start_flow <- function(data, description, idvar = NULL) {
  stopifnot(is.data.frame(data), is.character(description), length(description) == 1L)
  if (!is.null(idvar)) {
    stopifnot(is.character(idvar), length(idvar) == 1L)
    if (!idvar %in% names(data)) stop("`idvar` '", idvar, "' is not a column of `data`.", call. = FALSE)
    n <- dplyr::n_distinct(data[[idvar]])
  } else {
    n <- nrow(data)
  }
  flow <- tibble::tibble(
    `Patient set` = description,
    `# Remaining` = n,
    `# Excluded` = NA_integer_,
    `Excluded IDs` = NA_character_,
    `Criterion type` = NA_character_,
    `Criterion` = NA_character_
  )
  list(data = data, flow = flow, idvar = idvar)
}

#' @rdname flow
#' @export
add_to_flow <- function(flow, description, exclude = NULL, include = NULL,
                        idvar = flow$idvar) {
  stopifnot(is.list(flow), all(c("data", "flow") %in% names(flow)))
  stopifnot(is.character(description), length(description) == 1L)
  exclude <- rlang::enquo(exclude)
  include <- rlang::enquo(include)
  has_ex <- !rlang::quo_is_null(exclude)
  has_in <- !rlang::quo_is_null(include)
  if (has_ex == has_in) {
    stop("Supply exactly one of `exclude` and `include`.", call. = FALSE)
  }

  before <- flow$data
  if (has_ex) {
    crit_type <- "exclude"
    crit_text <- rlang::as_label(exclude)
    # NA in the exclusion criterion -> not known to be excluded -> kept
    drop <- rlang::eval_tidy(exclude, before)
    if (!is.logical(drop) || !length(drop) %in% c(1L, nrow(before))) {
      stop("`exclude` must evaluate to a logical vector.", call. = FALSE)
    }
    keep <- rep_len(!(drop %in% TRUE), nrow(before))
    after <- before[keep, , drop = FALSE]
  } else {
    crit_type <- "include"
    crit_text <- rlang::as_label(include)
    after <- dplyr::filter(before, !!include)
  }

  if (!is.null(idvar)) {
    if (!idvar %in% names(before)) stop("`idvar` '", idvar, "' is not a column of the data.", call. = FALSE)
    ids_out <- setdiff(unique(before[[idvar]]), unique(after[[idvar]]))
    n_remaining <- dplyr::n_distinct(after[[idvar]])
    n_excluded <- length(ids_out)
    excluded_ids <- if (n_excluded > 0L) paste(ids_out, collapse = ", ") else NA_character_
  } else {
    n_remaining <- nrow(after)
    n_excluded <- nrow(before) - nrow(after)
    excluded_ids <- NA_character_
  }

  flow$flow <- tibble::add_row(
    flow$flow,
    `Patient set` = description,
    `# Remaining` = n_remaining,
    `# Excluded` = n_excluded,
    `Excluded IDs` = excluded_ids,
    `Criterion type` = crit_type,
    `Criterion` = crit_text
  )
  flow$data <- after
  flow$idvar <- idvar
  flow
}

#' @rdname flow
#' @export
show_flow <- function(flow, title = "Subject selection", show_excluded_IDs = NULL) {
  rlang::check_installed("gt", reason = "to format the flow table.")
  if (is.null(show_excluded_IDs)) show_excluded_IDs <- !is.null(flow$idvar)
  stopifnot(is.logical(show_excluded_IDs), length(show_excluded_IDs) == 1L)
  tab <- flow$flow
  tab <- tab[, setdiff(names(tab), c("Criterion type", "Criterion")), drop = FALSE]
  if (!show_excluded_IDs) tab <- tab[, setdiff(names(tab), "Excluded IDs"), drop = FALSE]
  tab |>
    gt::gt() |>
    gt::tab_header(title = title) |>
    gt::sub_missing(missing_text = " ") |>
    gt::tab_style(style = gt::cell_text(weight = "bold"),
                  locations = gt::cells_column_labels())
}
