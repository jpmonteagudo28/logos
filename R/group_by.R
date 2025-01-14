#' Group and un-group data in a data frame
#'
#' These functions allow grouping and un-grouping of a `data.frame` by one or more variables.
#' Grouping enables the application of operations on subsets of data defined by the grouping variables.
#'
#' @param .data A `data.frame`. The data to be grouped or un-grouped.
#' @param ... One or more unquoted column names (or expressions) used for grouping.
#'   These columns must exist in the `data.frame`.
#'
#' @return
#' - For `group_by`, a `data.frame` with an additional class `grouped_data` and an attribute `groups` containing the grouping variables.
#' - For `ungroup`, the `data.frame` with groups removed, reverting it to a standard `data.frame` if no groups remain.
#'
#' @details
#' - `group_by` applies a grouping structure to a `data.frame`, enabling grouped operations.
#' - `ungroup` removes specified groups or all groups if none are specified.
#' - `apply_grouped_function` applies a function to each group and combines the results.
#' - `split_into_groups` internally splits the `data.frame` into subsets by the grouping variables.
#'
#' The grouping information is stored as an attribute `groups` in the returned `data.frame`. The grouped data can be printed, showing the grouping variables.
#'
#' @examples
#' # Example data frame
#' df <- data.frame(
#'   category = c("A", "A", "B", "B", "C", "C"),
#'   value = 1:6
#' )
#'
#' # Group by a single variable
#' grouped_df <- group_by(df, category)
#' print(grouped_df)
#'
#' # Ungroup the data frame
#' ungrouped_df <- ungroup(grouped_df)
#' print(ungrouped_df)
#'
#' @seealso
#' - [split_into_groups()] for splitting a `data.frame` into subsets by group.
#' - [apply_grouped_function()] for applying a function to each group.
#' - [print.grouped_data()] for custom printing of grouped data.
#'
#' @export
group_by <- function(.data, ...) {
  is_dataframe(.data)
  groups <- deparse_dots(...)
  unknown <- !(groups %in% colnames(.data))
  if (any(unknown)) stop("Invalid groups: ", extract(groups, unknown))
  structure(.data, class = c("grouped_data", class(.data)), groups = groups)
}

#' @param x A `data.frame`.
#' @rdname groups
#' @export
ungroup <- function(x, ...) {
  is_dataframe(x)
  rm_groups <- deparse_dots(...)
  groups <- attr(x, "groups")
  if (length(rm_groups) == 0L) rm_groups <- groups
  attr(x, "groups") <- extract(groups, !(groups %in% rm_groups))
  if (length(attr(x, "groups")) == 0L) {
    attr(x, "groups") <- NULL
    class(x) <- extract(class(x), !(class(x) %in% "grouped_data"))
  }
  x
}

apply_grouped_function <- function(.data, fn, ...) {
  groups <- attr(.data, "groups", exact = TRUE)
  grouped <- split_into_groups(.data, groups)
  res <- do.call(rbind, unname(lapply(grouped, fn, ...)))
  if (any(groups %in% colnames(res))) {
    class(res) <- c("grouped_data", class(res))
    attr(res, "groups") <- extract(groups, groups %in% colnames(res))
  }
  res
}

split_into_groups <- function(.data, groups) {
  class(.data) <- "data.frame"
  group_factors <- lapply(groups, function(x, .data) as.factor(extract2(.data, x)), .data)
  res <- split(x = .data, f = group_factors)
  res
}

#' @export
print.grouped_data <- function(x, ...) {
  class(x) <- "data.frame"
  print(x, ...)
  cat("\nGroups: ", paste(attr(x, "groups", exact = TRUE), collapse = ", "), "\n\n")
}
