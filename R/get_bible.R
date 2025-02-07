get_bible_version <- function(language,testament){

  language <- match.arg(language,
                        c("English","Hebrew","Greek"),
                        several.ok = FALSE)

  testament <- match.arg(testament,
                         c("Old","New","Both"),
                         several.ok = FALSE)

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
                                   "Old" = rasb_bible |>
                                     dplyr::filter(
                                       book %in% old_testament_books
                                     ),
                                   "New" = rasb_bible |>
                                     dplyr::filter(
                                       book %in% new_testament_books
                                     ),
                                   "Both" = rasb_bible
                            )
                          },
                          "Hebrew" = switch(testament,
                                            "Old" = old_testament,
                                            "New" = stop("The Leningrad Codex only includes the Old Testament."),
                                            "Both" = stop("The Leningrad Codex only includes the Old Testament.")
                          ),
                          "Greek" = switch(testament,
                                           "New" = new_testament,
                                           "Old" = stop("The Septuagint only includes the New Testament."),
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

  # Retrieve the full text of the chapter
  full_chapter <- retrieve_chapter(book,
                                   chapter,
                                   fraction,
                                   part,
                                   language,
                                   testament)


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

get_section <- function(section,bible){

  stopifnot(is.character(section))

  your_section <- section

  bible_sections <- unique(author_data$section)
  if(your_section %!in% bible_sections){
    stop("The section selected does not match any of the sections contained inthe 'author_data' dataset. Please, refer to this dataset for a complete list of sections")
  }

  books <- author_data |>
    dplyr::group_by(section) |>
    dplyr::filter(section == your_section) |>
    dplyr::ungroup() |>
    dplyr::select(books)


}
