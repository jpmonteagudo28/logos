`%!in%` <- Negate(`%in%`)

replace_char <- function(.input_string,
                         old_char,
                         new_char,
                         use_regex = FALSE) {

  if (old_char == "" || is.null(old_char)) {
    stop("Error: The character to replace cannot be empty.")
  }

  gsub(old_char,
       new_char,
       .input_string,
       fixed = !use_regex)

}
#---- ---- --- --- ---- ---- --- --- ---- ----#
first_to_upper <- function(.x, split_by = " ") {

  stopifnot(is.character(.x),
            is.character(split_by))

  result <- sapply(.x, function(x) {
    string <- strsplit(x, split_by)[[1]]
    paste(toupper(substring(string, 1, 1)),
          substring(string, 2),
          sep = "", collapse = " ")
  })

  return(result)
}
#---- ---- --- --- ---- ---- --- --- ---- ----#
# Kind of obvious-the opposite of the function
# above.
first_to_lower <- function(.x, split_by = " ") {

  stopifnot(is.character(.x),
            is.character(split_by))

  result <- sapply(.x, function(x) {
    string <- strsplit(x, split_by)[[1]]
    paste(tolower(substring(string, 1, 1)),
          substring(string, 2),
          sep = "", collapse = " ")
  })

  return(result)
}
#---- ---- --- --- ---- ---- --- --- ---- ----#

#---- ---- --- --- ---- ---- --- --- ---- ----#
is_whole <- function(book,chapter){

}

is_half <- function(x){

}

is_quarter <- function(x){

}
