# Suggest book match based on provided book string
suggest_closest_book <- function(book, book_list) {
  # Find close matches using fuzzy search
  matches <- agrep(book, book_list, value = TRUE, max.distance = 0.2)

  # If there is exactly one match, suggest it
  if (length(matches) == 1) {
    message(paste("Did you mean:", matches, "?"))
    return(matches)

    # If there are multiple matches, present up to 3 options
  } else if (length(matches) > 1) {
    # Limit to top 3 matches
    matches <- head(matches, 3)
    message("Multiple similar books found: ", paste(matches, collapse = ", "))

    # Prompt the user to choose
    selected_book <- menu(matches, title = "Please select the correct book:")

    # If the user selects a valid option, return the selected book
    if (selected_book > 0) {
      return(matches[selected_book])
    }
  }

  # Return NULL if no match was found or the user made no valid selection
  return(NULL)
}

validate_book <- function(book, book_list) {
  if (!is.null(book)) {
    if (!(book %in% book_list)) {
      # Try to find a close match
      suggestion <- suggest_closest_book(book, book_list)

      # If a suggestion exists, return it instead of stopping
      if (!is.null(suggestion)) {
        return(suggestion)
      }

      # Otherwise, stop execution
      stop("The book you provided doesn't match any books in the Old or New Testament. Please refer to the 'author_data' dataset for a complete list.")
    }
  }

  return(book)  # Return validated book
}
