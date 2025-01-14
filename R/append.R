#' Append a Column to a Data Frame
#'
#' Internal function to add a new column to a data frame and optionally position it
#' before or after a specified column.
#'
#' @param .data A data frame to which the column will be added.
#' @param new_col A vector containing the values of the new column to append.
#' The name of this vector will be used as the column name in the data frame.
#' @param .before (Optional) A string specifying the name of the column before which
#' the new column should be inserted. Defaults to `NULL`.
#' @param .after (Optional) A string specifying the name of the column after which
#' the new column should be inserted. Defaults to `NULL`.
#'
#' @return A data frame with the new column added at the specified position or at
#' the end if no position is specified.
#'
#' @keywords internal
#'
#'
#' @details
#' If both `.before` and `.after` are provided, `.before` takes precedence. If neither
#' is provided, the new column is appended at the end of the data frame.
#'
#' The column name is derived from the name of the input vector `new_col`.
#'

append_column <- function(.data,
                    new_col,
                    .before = NULL,
                    .after = NULL) {

  # Ensure the input is a data frame
  stopifnot(is.data.frame(.data))

  # Derive the column name from the name of the new_col vector
  new_col_name <- deparse(substitute(new_col))

  # Add the new column with the correct name to the data frame
  .data[[new_col_name]] <- new_col

  # Determine the position to insert the new column
  if (!is.null(.before)) {
    position <- match(.before, names(.data))
  } else if (!is.null(.after)) {
    position <- match(.after, names(.data)) + 1
  } else {
    # If no .before or .after specified,
    # just return with the new column at the end
    return(.data)

  }

  # Reorder columns based on the calculated position
  new_col_index <- which(names(.data) == new_col_name)
  .data <- .data[, append(1:(ncol(.data) - 1),
                          new_col_index, after = position - 1)]

  return(.data)
}




