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
                                           "Old Testament" = stop("The Septuagint only includes the New Testament."),
                                           "Both" = stop("The Septuagint only includes the New Testament.")
                          )
  )

  return(bible_version)
}


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
