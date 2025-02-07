#' Retrieve standardized book names based on biblical narrative styles or genres
#'
#' @description
#' Collect book names from RASB Bible, Leningrad Codex, or Septuagint based on narrative styles
#' outlined in 'author_data' dataset.
#' \itemize{
#' \item Law: Genesis – Deuteronomy
#' \item History: Joshua – Esther
#' \item Wisdom: Job – Songs of Solomon
#' \item Major Prophets: Isaiah - Daniel
#' \item Minor Prophets: Hosea – Malachi
#' \item Gospels: Matthew – John
#' \item Pauline Epistles: Romans – Philemon
#' \item General Epistles: Hebrews – Jude
#' \item Apocalyptic: Revelation}
#'
#' @param section a character vector or length 1 specifying the narrative section or genre in the Old or New Testament
#' @param testament a character vector of length 1 specifying the testament in which the section is found
#'
#' @return a character vector of standardized book names to be passed to other functions
#'
#' @examples
#' by_section("Gospels","new")
#'
by_section <- function(section,testament){

  stopifnot(is.character(section),
            is.character(testament))

  your_section <- section
  chosen_testament <- standardize_testament(testament)

  bible_sections <- unique(author_data$section)
  if(your_section %!in% bible_sections){
    stop("The section selected does not match any of the sections contained in the 'author_data' dataset. Please, refer to this dataset for a complete list of sections")
  }

  books <- author_data |>
    dplyr::group_by(section) |>
    dplyr::filter(section == your_section,
                  testament == chosen_testament) |>
    dplyr::ungroup() |>
    dplyr::select(books)

  books <- lapply(books, standardize_name)

  return(books)
}


#' Retrieve standardized book names based on biblical authors
#'
#' @description
#' Collect book names from RASB Bible, Leningrad Codex, or Septuagint based on biblical authors
#' outlined in 'author_data' dataset.
#' \itemize{
#' \item Moses: Genesis – Deuteronomy
#' \item Joshua: Joshua
#' \item Unkown: Judges, Ruth, 1 & 2 Kings, 1 & Chronicles, Esther and Job
#' \item Samuel/Unknown: 1 & 2 Samuel
#' \item Ezra: Ezra
#' \item Nehemiah: Nehemiah
#' \item David/Various: Psalms
#' \item Solomon: Proverbs, Ecclesiastes, Song of Solomon
#' \item Isaiah: Isaiah
#' \item Jeremiah: Jeremiah and Lamentations
#' \item Ezekiel: Ezekiel
#' \item Daniel: Daniel
#' \item Hosea: Hosea
#' \item Joel: Joel
#' \item Amos: Amos
#' \item Obadiah: Obadiah
#' \item Jonah: Jonah
#' \item Micah: Micah
#' \item Nahum: Nahum
#' \item Habakkuk: Habakkuk
#' \item Zephaniah: Zephaniah
#' \item Haggai: Haggai
#' \item Zechariah: Zechariah
#' \item Malachi: Malachi
#' \item Matthew: Gospel of Matthew
#' \item Mark: Gospel of Mark
#' \item Luke: Gospel of Luke and Acts
#' \item John: Gospel of John, Johanine epistles and Revelation
#' \item Paul: Pauline epistles
#' \item James: James
#' \item Peter: 1 and 2 Peter
#' \item Jude: Jude
#' \item Unknown: Hebrews}
#'
#' @param author a character vector or length 1 specifying the biblical author in the Old or New Testament
#' @param testament a character vector of length 1 specifying the testament/time period in which the author is found
#'
#' @return a character vector of standardized book names to be passed to other functions
#'
#' @examples
#' by_author("Matthew","new")
by_author <- function(author, testament){

  stopifnot(is.character(author),
            is.character(testament))

  your_author <- author
  chosen_testament <- standardize_testament(testament)

  by_author <- unique(author_data$author)
  if(your_author %!in% by_author){
    stop("The section selected does not match any of the sections contained in the 'author_data' dataset. Please, refer to this dataset for a complete list of sections")
  }

  books <- author_data |>
    dplyr::group_by(author) |>
    dplyr::filter(author == your_author,
                  testament == chosen_testament) |>
    dplyr::ungroup() |>
    dplyr::select(books)

  books <- lapply(books, standardize_name)

  return(books)
}

#' Retrieve standardized book names based on probable writing period
#'
#' @description
#' Collect book names from RASB Bible, Leningrad Codex, or Septuagint based on the dates
#' outlined in 'author_data' dataset.
#'
#' @param section a character vector or length 1, written as a date range, "1445 - 1405 BC", specifying the probable writing period of the Old or New Testament books
#' @param testament a character vector of length 1 specifying the testament corresponding to the provided date range
#'
#' @return a character vector of standardized book names to be passed to other functions
#'
#' @examples
#' by_date("65 AD","new")
#' by_date("1445 - 1405 BC", "old")
by_date <- function(date_range,testament){

  default_date <- unique(author_data$date)
  chosen_testament <- standardize_testament(testament)

  # Define regex patterns for Old and New Testament
  old_regex <- "\\bc\\.\\s*(\\d{3,4})(?:-(\\d{3,4}))?\\s*BC|Unknown\\b"
  new_regex <- "\\b(c\\.\\s*\\d{1,2}(?:-\\d{1,2})?\\s*AD)|Unknown\\b"

  # Select appropriate regex based on testament
  regex <- if (chosen_testament == "New Testament") new_regex else old_regex

  # Find matching dates
  matches <- grep(regex, default_date, value = TRUE)

  # If date_range is provided, extract numeric values and filter matches
  if (!is.null(date_range)) {
    range_values <- as.numeric(unlist(regmatches(date_range, gregexpr("\\d+", date_range))))

    if (length(range_values) > 0) {
      # Parse extracted date values
      start_date <- range_values[1]
      end_date <- ifelse(length(range_values) > 1, range_values[2], start_date)

      # Filter matched dates within the range
      matches <- matches[sapply(matches, function(date) {
        num_values <- as.numeric(unlist(regmatches(date, gregexpr("\\d+", date))))
        if (length(num_values) > 0) {
          book_start <- num_values[1]
          book_end <- ifelse(length(num_values) > 1, num_values[2], book_start)

          # Check if the book's date range overlaps with the input date range
          return(!(book_end < start_date || book_start > end_date))
        }
        return(FALSE)
      })]
    }
  }

  books <- author_data |>
    dplyr::group_by(author) |>
    dplyr::filter(date %in% matches,
                  testament == chosen_testament) |>
    dplyr::ungroup() |>
    dplyr::select(books)

  books <- lapply(books, standardize_name)

  return(books)
}
