# Code inspired by https://github.com/nathaneastwood/poorman/blob/master/R/glimpse.R
#' Take a Quick Overview of Your Data
#'
#' `peek()` provides a transposed view of your dataset: columns are displayed vertically, while the data for each column
#' is shown horizontally. This layout allows you to inspect all the columns of a `data.frame` at a glance. It serves as a
#' convenient wrapper around [utils::str()], with the added benefit of invisibly returning the input object, making it
#' suitable for use in data pipelines.
#'
#' @param x The object to be inspected.
#' @param width `integer(1)`. Specifies the maximum width of the output.
#' @param ... Additional arguments to pass to [utils::str()].
#'
#' @return
#' The input object `x`, returned invisibly.
#'
#' @examples
#' peek(mtcars)

#' @export
peek <- function(x, width = getOption("width"), ...) {
  UseMethod("peek")
}
#' @export
peek.default <- function(x, ...) {
  cat("<Default object of class ", class(x)[1], ">\n", sep = "")
  invisible(x)
}

#' @export
peek.data.frame <- function(x, ...) {
  # Get the number of rows and columns
  n_rows <- nrow(x)
  n_cols <- ncol(x)

  # Display rows and columns
  display_rows_and_columns(n_rows, n_cols)

  # Calculate the maximum column name length
  max_col_name_length <- find_colname_length(x)

  # Display each column with dynamically calculated vector length
  display_columns(x, max_col_name_length)

  invisible(x)
}

# Function to display number of rows and columns
display_rows_and_columns <- function(n_rows, n_cols) {
  cat("Rows:", n_rows, "\n")
  cat("Columns:", n_cols, "\n")
  cat("-------------------------\n")
}

# Function to display columns with name, class, and first few elements
display_columns <- function(x, max_col_name_length) {
  # Create a mapping of class names to abbreviations
  class_abbreviations <- get_class_abbreviations()

  # Loop over columns and display each one
  for (col_name in names(x)) {
    # Get the first few values for each column
    col_values <- utils::head(x[[col_name]], get_display_length(x[[col_name]]))
    col_class <- class(x[[col_name]])[1]

    # Use abbreviation if available, otherwise keep the full class name
    col_class_abbr <- class_abbreviations[[col_class]] %||% paste("<", col_class, ">")

    # Convert factors to character for display
    if (is.factor(x[[col_name]])) {
      col_values <- as.character(col_values)
    }

    # Display the column with proper alignment
    display_column(col_name, col_class_abbr, col_values, max_col_name_length)
  }
}

# Function to get the appropriate number of
# elements to display for each vector

get_display_length <- function(x) {

  # Default value for vec.len from options("str") (default is 4)
  vec_len <- getOption("str")$vec.len %||% 4

  # Adjust based on the type of vector
  if (is.factor(x)) {
    return(vec_len * 2)
  } else if (is.numeric(x) || is.integer(x)) {
    return(vec_len * 3)
  } else {
    return(vec_len * 2)
  }
}

# Function to get class abbreviations for common types
get_class_abbreviations <- function() {
  return(c(
    "factor" = "<fct>",
    "numeric" = "<dbl>",
    "character" = "<chr>",
    "integer" = "<int>",
    "logical" = "<lgl>",
    "Date" = "<date>",
    "POSIXct" = "<ct>"
  ))
}

find_colname_length <- function(x) {

  max_length<- max(nchar(names(x))) + 5

  return(max_length)
}

# Function to display individual column with class in lighter color
display_column <- function(col_name, col_class_abbr, col_values, max_col_name_length) {
  # ANSI escape code for light gray color
  light_color <- "\033[38;5;145m"
  reset_color <- "\033[39m"

  # Ensure that the column names have values
  col_name_length <- if (length(col_name) > 0) nchar(col_name) else 0

  # Fallback if max column name length is invalid (e.g., empty column names)
  max_col_name_length <- ifelse(is.finite(col_name_length), max(col_name_length, max_col_name_length), max_col_name_length)

  # Format and print the column details with aligned output
  cat(sprintf("$ %-*s %s%s%s %s...\n",
              max_col_name_length, col_name,
              light_color, col_class_abbr, reset_color,
              paste(col_values, collapse = ", ")))
}
