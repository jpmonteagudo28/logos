select_passage <- function(book = NULL,
                           chapter = NULL,
                           fraction = 1,# option for whole book, half, quarter
                           part = 1,
                           verse = NULL,
                           by = NULL,
                           testament = NULL,
                           language = "English"){

  # We definitely need a book
  # and language to provide
  # a valid output.
  stopifnot(is.character(language),
            is.character(testament))

  language <- match.arg(language,
                        c("English","Hebrew","Greek"),
                        several.ok = FALSE)

  testament <- match.arg(testament,
                         c("Old","New","Both"),
                         several.ok = FALSE)

  # Dealing with the by argument
  by <- match.arg(by,
                  c("author","section",NULL),
                  several.ok = FALSE)


  # Manage book input, if provided
  if (!is.null(book)) {
    book <- validate_book(book,author_data$books)
  }



  # What to do if chapter and verse are included
  if(!is.null(chapter) && !is.numeric(chapter)){
    stop("verse must be a numeric vector of length equal or greater than 1")
  } else {
    chapter <- chapter
  }

  if(!is.null(verse) && !is.numeric(verse)){
    stop("verse must be a numeric vector of length equal or greater than 1")
  } else {
    verse <- verse
  }


  if(!is.null(by) && !(is.null(book) || is.null(chapter) || is.null(verse))){
    stop("If choosing to display books by author or section, the 'book','chapter' and 'verse' arguments must be NULL")
  } else{
      # if by is an author names, grab the author
      if(by %in% author_data$author){
        author <- by
        # otherwise, we assume it matches a section of the Bible
      } else if(by %in% author_data$section){
        section <- by
        # if neither is true, well…, the user needs to check himself ;)
      } else if(by %in% author_data$date){
        date_range <- by
      } else {
        stop("The author or section you chose doesn't match the biblical authors or sections in 'author_data'. Please, check the 'author_data' dataset for a full list of authors and section.")
      }
  }

  # Now that we have identified, book, chapter



  # output should just be the
  # text(character string),
  # not a data frame
}
