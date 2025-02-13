#' Retrieve a specific chapter or portion of a chapter from the Bible
#'
#' This function retrieves text from a specified book and chapter, with optional filtering
#' by verse, partitioning, and language selection.
#'
#' @param book A character string specifying the book of the Bible.
#' @param chapter A numeric or character vector specifying the chapter(s) to retrieve.
#' @param verse An optional numeric vector specifying specific verses to retrieve.
#' @param fraction A numeric value indicating how many equal parts to divide the chapter into.
#' @param part A numeric value specifying which part to return (must be between 1 and `fraction`).
#' @param language A character string specifying the language of the Bible text. Options are "English", "Hebrew", or "Greek".
#' @param testament A character string specifying whether to retrieve from the Old or New Testament.
#'
#' @return A character vector containing the retrieved Bible text.
#' @export
#'
retrieve_chapter <- function(book,
                             chapter = NULL,
                             verse = NULL,
                             fraction = NULL,
                             part = NULL,
                             language,
                             testament) {

  language <- match.arg(language,
                        c("English","Hebrew","Greek"),
                        several.ok = FALSE)

  testament <- standardize_testament(testament)

  bible_data <- get_bible_version(language = language,testament = testament)

  if (is.null(bible_data) || nrow(bible_data) == 0) {
    stop("Error: No Bible data found for the specified language and testament.")
  }

  # Validate book name using fuzzy matching
  validated_books <- validate_book(book, bible_data$book)
  if (is.null(validated_books)) stop("Book not found. Please check your input.")

  # Filter for the specified book(s) and, if provided, chapter(s)
  chapter_data <- bible_data |>
    dplyr::filter(book %in% validated_books)


  if (!is.null(chapter)) {
    chapter_data <- chapter_data |>
      dplyr::filter(chapter %in% !!chapter)
  }

  if (!is.null(verse)) {
    chapter_data <- chapter_data |>
      dplyr::filter(verse %in% !!verse)
  }

  # If nothing remains, return error
  if (nrow(chapter_data) == 0) {
    stop("Error: No matching verses found for the given book and chapter.")
  }

  # Sort by verse
  chapter_data <- chapter_data |> dplyr::arrange(verse)

  # Get total verses in the filtered subset
  total_verses <- nrow(chapter_data)

  if (!is.null(fraction) || !is.null(part)) {
    if (is.null(fraction) || is.null(part)) {
      stop("Both 'fraction' and 'part' must be provided together.")
    }
    if (fraction < 1 || part < 1 || part > fraction) {
      stop("Invalid fraction or part. Ensure that fraction >= 1 and 1 <= part <= fraction.")
    }

    # Get total verses
    total_verses <- nrow(chapter_data)
    section_size <- ceiling(total_verses / fraction)
    start_verse <- (part - 1) * section_size + 1
    end_verse <- min(part * section_size, total_verses)

    # Retrieve only the requested portion
    chapter_data <- chapter_data |> dplyr::slice(start_verse:end_verse)
  }

  return(chapter_data$text)
}
