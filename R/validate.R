#' Suggest the closest matching book name
#'
#' This function suggests the closest matching book name based on the input string.
#' It uses approximate string matching to find similar book names from the provided list.
#'
#' @param book A character vector of book names to check.
#' @param book_list A character vector of valid book names to match against.
#'
#' @return A character vector of suggested book names or NA if no close match is found.
#' @export
#' @keywords internal
#'
suggest_closest_book <- function(book, book_list) {
  book_list <- unique(book_list)

  # Apply agrep to each book separately
  suggestions <- sapply(book, function(x) {
    matches <- agrep(x, book_list, value = TRUE, max.distance = 0.2)

    # If exactly one match, return it immediately
    if (length(matches) == 1) {
      message(paste("Did you mean:", matches, "?"))
      return(matches)
    }

    # If multiple matches, force selection
    while (length(matches) > 1) {
      message("Multiple similar books found for '", x, "': ", paste(matches, collapse = ", "))

      # If interactive mode, prompt user
      if (interactive()) {
        selected_book <- utils::menu(matches, title = paste("Please select the correct book for:", x))

        if (selected_book > 0) {
          return(matches[selected_book])
        }
      }

      # If not interactive or no selection, automatically choose first match
      message("No selection made. Defaulting to:", matches[1])
      return(matches[1])
    }

    # No match found
    message("No close matches found for:", x)
    return(NA_character_)
  }, USE.NAMES = FALSE)

  return(suggestions)
}



#' Validate book names against a reference list
#'
#' This function checks if the provided book names exist in a given list and suggests the closest match if not.
#'
#' @param book A character vector, list, or data frame containing book names.
#' @param book_list A character vector of valid book names.
#'
#' @return A character vector of validated or suggested book names.
#' @export
#' @keywords internal

validate_book <- function(book, book_list) {

  if (is.null(book)) {
    return(book)  # Return early if book is NULL
  }

  # Only works for data frames of ncol = 1. It gets messy for more than 1 column
  if(is_nested(book) && !is.data.frame(book)){
  stop("Unnest your list before proceeding any further")
  } else if(is.list(book) || is.data.frame(book)){
    books <- unlist(book)
  }

  book <- standardize_name(book)

  # If all books exist in book_list, return them as is
  if (all(book %in% book_list)) {
    return(book)
  }

  # Find suggestions for books not in book_list
  suggestion <- if (length(book) == 1L) {
    suggest_closest_book(book, book_list)
  } else {
    sapply(book, function(x) suggest_closest_book(x, book_list), USE.NAMES = FALSE)
  }

  # If suggestions exist, return them
  if (!is.null(suggestion)) {
    return(suggestion)
  }

  # Stop execution if no valid match is found
  stop("The book you provided doesn't match any books in the Old or New Testament.
       Please refer to the 'author_data' dataset for a complete list.")
}

#' Standardize book names to a common format
#'
#' This function converts book names to their standardized abbreviations using a predefined lookup table.
#'
#' @param book A character vector of book names.
#'
#' @return A character vector of standardized book names.
#' @export
#' @keywords internal

standardize_name <- function(book) {
  # Define a lookup table with known variations and target format
  book_map <- c(
    # Old Testament
    "Genesis" = "Gen", "Gen" = "Gen", "Gn" = "Gen",
    "Exodus" = "Exod", "Exo" = "Exod", "Ex" = "Exod",
    "Leviticus" = "Lev", "Lev" = "Lev", "Lv" = "Lev",
    "Numbers" = "Num", "Num" = "Num", "Nm" = "Num", "Numb" = "Num",
    "Deuteronomy" = "Deut", "Deu" = "Deut", "Dt" = "Deut",
    "Joshua" = "Josh", "Jsh" = "Josh", "Jos" = "Josh",
    "Judges" = "Judg", "Jdg" = "Judg", "Jdgs" = "Judg",
    "Ruth" = "Ruth", "Ru" = "Ruth",
    "1 Samuel" = "1Sam", "1Sa" = "1Sam", "1sa" = "1Sam",
    "2 Samuel" = "2Sam", "2Sa" = "2Sam", "2sa" = "2Sam",
    "1 Kings" = "1Kgs", "1Ki" = "1Kgs", "1ki" = "1Kgs",
    "2 Kings" = "2Kgs", "2Ki" = "2Kgs", "2ki" = "2Kgs",
    "1 Chronicles" = "1Chron", "1Ch" = "1Chron", "1ch" = "1Chron",
    "2 Chronicles" = "2Chron", "2Ch" = "2Chron", "2ch" = "2Chron",
    "Ezra" = "Ezra", "Ezr" = "Ezra",
    "Nehemiah" = "Neh", "Ne" = "Neh",
    "Esther" = "Esth", "Est" = "Esth", "Es" = "Esth",
    "Job" = "Job", "Jb" = "Job",
    "Psalms" = "Ps", "Psalm" = "Ps", "Psa" = "Ps", "Psm" = "Ps", "Pss" = "Ps",
    "Proverbs" = "Prov", "Pro" = "Prov", "Pr" = "Prov",
    "Ecclesiastes" = "Eccl", "Ecc" = "Eccl", "Ec" = "Eccl",
    "Song of Solomon" = "Song", "Song" = "Song", "SoS" = "Song", "Canticles" = "Song",
    "Isaiah" = "Isa", "Is" = "Isa",
    "Jeremiah" = "Jer", "Jr" = "Jer",
    "Lamentations" = "Lam", "Lament" = "Lam", "La" = "Lam",
    "Ezekiel" = "Ezek", "Ezk" = "Ezek", "Ez" = "Ezek",
    "Daniel" = "Dan", "Dn" = "Dan",
    "Hosea" = "Hos", "Ho" = "Hos",
    "Joel" = "Joel", "Jl" = "Joel",
    "Amos" = "Amos", "Am" = "Amos",
    "Obadiah" = "Obad", "Oba" = "Obad", "Ob" = "Obad",
    "Jonah" = "Jonah", "Jnh" = "Jonah", "Jon" = "Jonah",
    "Micah" = "Mic", "Mi" = "Micah",
    "Nahum" = "Nah", "Na" = "Nahum",
    "Habakkuk" = "Hab", "Hb" = "Habakkuk",
    "Zephaniah" = "Zeph", "Zep" = "Zeph", "Zp" = "Zephaniah",
    "Haggai" = "Hag", "Hg" = "Haggai",
    "Zechariah" = "Zech", "Zc" = "Zechariah",
    "Malachi" = "Mal", "Ml" = "Malachi",

    # New Testament
    "Matthew" = "Matt", "Mth" = "Matt", "Mt" = "Matt",
    "Mark" = "Mark", "Mrk" = "Mark", "Mk" = "Mark",
    "Luke" = "Luke", "Luk" = "Luke", "Lk" = "Luke",
    "John" = "John", "Jhn" = "John", "Jn" = "John",
    "Acts" = "Acts", "Act" = "Acts", "Ac" = "Acts",
    "Romans" = "Rom", "Ro" = "Rom", "Rm" = "Rom",
    "1 Corinthians" = "1Cor", "1Co" = "1Cor", "1co" = "1Cor",
    "2 Corinthians" = "2Cor", "2Co" = "2Cor", "2co" = "2Cor",
    "Galatians" = "Gal", "Ga" = "Galatians",
    "Ephesians" = "Eph", "Ep" = "Ephesians",
    "Philippians" = "Phil", "Php" = "Philippians", "Phl" = "Phil",
    "Colossians" = "Col", "Cl" = "Colossians",
    "1 Thessalonians" = "1Thess", "1Th" = "1Thess", "1th" = "1Thess",
    "2 Thessalonians" = "2Thess", "2Th" = "2Thess", "2th" = "2Thess",
    "1 Timothy" = "1Tim", "1Ti" = "1Tim", "1ti" = "1Tim",
    "2 Timothy" = "2Tim", "2Ti" = "2Tim", "2ti" = "2Tim",
    "Titus" = "Titus", "Ti" = "Titus",
    "Philemon" = "Phlm", "Phm" = "Phlm",
    "Hebrews" = "Heb", "Hb" = "Hebrews",
    "James" = "Jas", "Jms" = "Jas", "Jam" = "Jas",
    "1 Peter" = "1Pet", "1Pe" = "1Pet", "1pe" = "1Pet",
    "2 Peter" = "2Pet", "2Pe" = "2Pet", "2pe" = "2Pet",
    "1 John" = "1John", "1Jn" = "1John", "1jn" = "1John",
    "2 John" = "2John", "2Jn" = "2John", "2jn" = "2John",
    "3 John" = "3John", "3Jn" = "3John", "3jn" = "3John",
    "Jude" = "Jude", "Jde" = "Jude", "Jud" = "Jude",
    "Revelation" = "Rev", "Rv" = "Rev"
  )

  # Convert input book names
  transformed <- ifelse(book %in% names(book_map), book_map[book], book)

  return(transformed)
}

#' Standardize Bible testaments to a common format
#'
#' This function converts testament to their standardized abbreviations using a predefined lookup table.
#'
#' @param testament A character vector of testament names.
#'
#' @return A character vector of standardized testament names.
#' @export
#' @keywords internal

standardize_testament <- function(testament){

  testament_map <- c("Old" = "Old Testament",
                     "old" = "Old Testament",
                     "old testament" = "Old Testament",
                     "New" = "New Testament",
                     "new" = "New Testament",
                     "new testament" = "New Testament",
                     "both" = "Both")

  transformed <- ifelse(testament %in% names(testament_map),testament_map[testament],testament)

  return(transformed)
}
