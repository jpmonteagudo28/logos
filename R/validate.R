# Suggest book match based on provided book string
suggest_closest_book <- function(book, book_list) {

  book_list <- unique(book_list)

  # Apply agrep to each book separately
  suggestions <- sapply(book, function(x) {
    matches <- agrep(x, book_list, value = TRUE, max.distance = 0.2)

    # If exactly one match, return it
    if (length(matches) == 1) {
      message(paste("Did you mean:", matches, "?"))
      return(matches)

      # If multiple matches, return up to 3 options
    } else if (length(matches) > 1) {
      matches <- head(matches, 3)
      message("Multiple similar books found for '", x, "': ", paste(matches, collapse = ", "))

      # Prompt user to select (only in interactive mode)
      if (interactive()) {
        selected_book <- menu(matches, title = paste("Please select the correct book for:", x))

        if (selected_book > 0) {
          return(matches[selected_book])
        }
      }

      # If not interactive or no selection, return first match
      return(matches[1])
    }

    # No match found
    return(NA_character_)
  }, USE.NAMES = FALSE)

  return(suggestions)
}


# Validation of book names
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


standardize_name <- function(book) {
  # Define a lookup table with known variations and target format
  book_map <- c(
    "Genesis" = "Gen", "Exo" = "Exod", "Exodus" = "Exod", "Lev" = "Lev", "Leviticus" = "Lev",
    "Numbers" = "Num", "Num" = "Num", "Deuteronomy" = "Deut", "Deu" = "Deut",
    "Joshua" = "Josh", "Jsh" = "Josh", "Judges" = "Judg", "Jdg" = "Judg",
    "Ruth" = "Ruth", "1 Samuel" = "1Sam", "1Sa" = "1Sam", "1sa" = "1Sam", "2 Samuel" = "2Sam",
    "2Sa" = "2Sam", "2sa" = "2Sam", "1 Kings" = "1Kgs", "1Ki" = "1Kgs", "1ki" = "1Kgs",
    "2 Kings" = "2Kgs", "2Ki" = "2Kgs", "2ki" = "2Kgs", "1 Chronicles" = "1Chron",
    "1Ch" = "1Chron", "1ch" = "1Chron", "2 Chronicles" = "2Chron", "2Ch" = "2Chron",
    "2ch" = "2Chron", "Ezra" = "Ezra", "Ezr" = "Ezra", "Nehemiah" = "Neh", "Esther" = "Esth",
    "Est" = "Esth", "Job" = "Job", "Psalms" = "Ps", "Psalm" = "Ps", "Psa" = "Ps",
    "Proverbs" = "Prov", "Pro" = "Prov", "Ecclesiastes" = "Eccl", "Ecc" = "Eccl",
    "Song of Solomon" = "Song", "Song" = "Song", "Isaiah" = "Isa", "Jeremiah" = "Jer",
    "Lamentations" = "Lam", "Lament" = "Lam", "Ezekiel" = "Ezek", "Ezk" = "Ezek",
    "Daniel" = "Dan", "Hosea" = "Hos", "Joel" = "Joel", "Amos" = "Amos",
    "Obadiah" = "Obad", "Oba" = "Obad", "Jonah" = "Jonah", "Jna" = "Jonah", "Micah" = "Mic",
    "Nahum" = "Nah", "Habakkuk" = "Hab", "Zephaniah" = "Zeph", "Zph" = "Zeph",
    "Haggai" = "Hag", "Zechariah" = "Zech", "Malachi" = "Mal",

    # New Testament
    "Matthew" = "Matt", "Mth" = "Matt", "Mark" = "Mark", "Mrk" = "Mark", "Luke" = "Luke",
    "Luk" = "Luke", "John" = "John", "Jhn" = "John", "Acts" = "Acts", "Act" = "Acts",
    "Romans" = "Rom", "1 Corinthians" = "1Cor", "1Co" = "1Cor", "1co" = "1Cor",
    "2 Corinthians" = "2Cor", "2Co" = "2Cor", "2co" = "2Cor", "Galatians" = "Gal",
    "Ephesians" = "Eph", "Philippians" = "Phil", "Phl" = "Phil", "Colossians" = "Col",
    "1 Thessalonians" = "1Thess", "1Th" = "1Thess", "1th" = "1Thess", "2 Thessalonians" = "2Thess",
    "2Th" = "2Thess", "2th" = "2Thess", "1 Timothy" = "1Tim", "1Ti" = "1Tim",
    "1ti" = "1Tim", "2 Timothy" = "2Tim", "2Ti" = "2Tim", "2ti" = "2Tim",
    "Titus" = "Titus", "Philemon" = "Phlm", "Phm" = "Phlm", "Hebrews" = "Heb",
    "James" = "Jas", "Jms" = "Jas", "1 Peter" = "1Pet", "1Pe" = "1Pet", "1pe" = "1Pet",
    "2 Peter" = "2Pet", "2Pe" = "2Pet", "2pe" = "2Pet", "1 John" = "1John",
    "1Jn" = "1John", "1jn" = "1John", "2 John" = "2John", "2Jn" = "2John",
    "2jn" = "2John", "3 John" = "3John", "3Jn" = "3John", "3jn" = "3John",
    "Jude" = "Jude", "Jde" = "Jude", "Revelation" = "Rev"
  )

  # Convert input book names
  transformed <- ifelse(book %in% names(book_map), book_map[book], book)

  return(transformed)
}


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
