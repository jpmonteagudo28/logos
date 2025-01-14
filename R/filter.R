#' Filter
#'
#' @param .data A `data.frame`.
#' @param ... Expressions used to filter the data by.
filter <- function(.data, ...) {
  is_dataframe(.data)
  UseMethod("filter")
}

#' @export
filter.default <- function(.data, ...) {
  conditions <- paste(deparse_dots(...), collapse = " & ")
  extract(.data, with(.data, eval(parse(text = conditions))), )
}

#' @export
filter.grouped_data <- function(.data, ...) {
  apply_grouped_function(.data, "filter", ...)
}
