select_passage <- function(book = NULL,
                           chapter = NULL, # option for whole book, half, quarter
                           verse = NULL,   # option to match chapter selection
                           by = NULL,
                           testament = NULL,
                           language = NULL){

  # We definitely need a book
  # and language to provide
  # a valid output.
  stopifnot(is.character(book),
            is.character(language))

  by <- match.arg(by,
                  c("author","section","dates"),
                  several.ok = FALSE)


  # output should just be the
  # text(character string),
  # not a data frame
}

