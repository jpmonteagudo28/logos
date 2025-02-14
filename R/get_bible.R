#' Get Bible Version
#'
#' Retrieves the Bible data filtered by the specified language and testament.
#'
#' @param language A character string specifying the language of the Bible.
#'   Must be one of \code{"English"}, \code{"Hebrew"}, or \code{"Greek"}.
#' @param testament A character string specifying the testament. This input is standardized using
#'   \code{standardize_testament()} and should typically be \code{"Old Testament"}, \code{"New Testament"},
#'   or \code{"Both"}.
#'
#' @details
#' This function filters the underlying Bible dataset based on the specified \code{language} and \code{testament}.
#' For English, it divides the dataset into Old and New Testaments by using predefined book ranges from the
#' \code{rasb_bible} dataset. For Hebrew and Greek, it refers to the \code{old_testament} and \code{new_testament}
#' datasets respectively, and will produce an error if an unsupported combination is requested.
#'
#' @return A data frame containing the Bible data corresponding to the selected language and testament.
#' @export
#' @keywords internal

get_bible_version <- function(language,testament){

  language <- match.arg(language,
                        c("English","Hebrew","Greek"),
                        several.ok = FALSE)

  testament <- standardize_testament(testament)

  # Define book ranges for the Old and New Testament
  books <- unique(rasb_bible$book)
  old_testament_books <- books[1:39]   # Books 1 to 39
  new_testament_books <- books[40:66]  # Books 40 to 66


  # Filter the dataset based on language and testament
  bible_version <- switch(language,
                          "English" = {
                            if (!"book" %in% colnames(rasb_bible)) {
                              stop("The provided dataset must contain a 'book' column.")
                            }
                            switch(testament,
                                   "Old Testament" = rasb_bible |>
                                     dplyr::filter(
                                       book %in% old_testament_books
                                     ),
                                   "New Testament" = rasb_bible |>
                                     dplyr::filter(
                                       book %in% new_testament_books
                                     ),
                                   "Both" = rasb_bible
                            )
                          },
                          "Hebrew" = switch(testament,
                                            "Old Testament" = old_testament,
                                            "New Testament" = stop("The Leningrad Codex only includes the Old Testament."),
                                            "Both" = stop("The Leningrad Codex only includes the Old Testament.")
                          ),
                          "Greek" = switch(testament,
                                           "New Testament" = new_testament,
                                           "Old Testament" = stop("The Greek New Testament only includes the New Testament."),
                                           "Both" = stop("The Greek New Testament only includes the New Testament.")
                          )
  )

  return(bible_version)
}

#' Get Fraction of a Chapter or Book
#'
#' Extracts a specified fraction of a chapter from a given book or a portion of a book.
#'
#' @param book A character string or vector specifying the book(s) from which the chapter is retrieved.
#' @param chapter A numeric vector indicating the chapter number(s) to be processed.
#' @param fraction A numeric value representing the total number of sections into which the chapter should be divided.
#'   Must be an integer greater than or equal to 1.
#' @param part A numeric value indicating which section of the divided chapter to return.
#'   Must satisfy \code{1 <= part <= fraction}.
#' @param language An optional character string indicating the language of the Bible.
#'   Typically one of \code{"English"}, \code{"Hebrew"}, or \code{"Greek"}. Default is \code{NULL}.
#' @param testament An optional character string specifying the testament.
#'   Should be \code{"Old Testament"}, \code{"New Testament"}, or \code{"Both"}. Default is \code{NULL}.
#'
#' @details
#' This function validates the \code{fraction} and \code{part} parameters, retrieves the full chapter text using
#' \code{retrieve_chapter()}, and calculates the corresponding section of the chapter to return. The chapter is divided
#' into equal parts based on the number of verses, and the function extracts the verses corresponding to the requested part.
#'
#' @return A character vector containing the verses from the specified section of the chapter.
#' @export
#' @keywords internal
#'
get_fraction <- function(book,
                        chapter,
                        fraction,
                        part,
                        language = NULL,
                        testament = NULL) {

  # Validate fraction and part
  if (fraction < 1 || part < 1 || part > fraction) {
    stop("Invalid fraction or part. Ensure that fraction >= 1 and 1 <= part <= fraction.")
  }

  # Retrieve the full text of the chapter
  full_chapter <- retrieve_chapter(book,
                                   chapter,
                                   fraction = 1,
                                   part = 1,
                                   language = language,
                                   testament = testament)


  # Determine the total number of verses
  num_verses <- length(full_chapter)

  # Calculate section size
  section_size <- ceiling(num_verses / fraction)

  # Determine start and end indices
  start_verse <- (part - 1) * section_size + 1
  end_verse <- min(part * section_size, num_verses)

  # Extract and return the requested section
  return(full_chapter[start_verse:end_verse])
}
