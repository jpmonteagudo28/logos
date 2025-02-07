retrieve_chapter <- function(book,
                             chapter,
                             fraction = 1,
                             part = 1,
                             language,
                             testament) {

  language <- match.arg(language,
                        c("English","Hebrew","Greek"),
                        several.ok = FALSE)

  testament <- match.arg(testament,
                         c("Old","New","Both"),
                         several.ok = FALSE)

  bible_data <- get_bible_version(language,testament)

  # Verse is a numeric column
  verse <- bible_data$verse

  # Validate book name using fuzzy matching
  book <- suggest_closest_book(book, unique(bible_data$book))
  if (is.null(book)) stop("Book not found. Please check your input.")

  # Filter for the specified book and chapter
  chapter_data <- bible_data |>
    dplyr::filter(book == !!book, chapter == !!chapter) |>
    dplyr::arrange(verse)

  # Get total verses in the chapter
  total_verses <- nrow(chapter_data)

  # Validate fraction and part
  if (fraction < 1 || part < 1 || part > fraction) {
    stop("Invalid fraction or part. Ensure that fraction >= 1 and 1 <= part <= fraction.")
  }

  # Determine verse range for the requested part
  section_size <- ceiling(total_verses / fraction)
  start_verse <- (part - 1) * section_size + 1
  end_verse <- min(part * section_size, total_verses)

  # Retrieve only the requested portion
  chapter_data <- chapter_data |>
    dplyr::slice(start_verse:end_verse)

  return(chapter_data$text)
}
