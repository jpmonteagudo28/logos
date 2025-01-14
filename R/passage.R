select_passage <- function(book = NULL,
                           chapter = NULL, # option for whole book, half, quarter
                           verse = NULL,   # option to match chapter selection
                           by = NULL,
                           testament = NULL,
                           language = NULL){

  # We definitely need a book and language to provide
  # a valid output.
  stopifnot(is.character(book),
            is.character(language))

  by <- match.arg(by,
                  c("author","section","dates","literature"),
                  several.ok = FALSE)

  language <- match.arg(language,
                        c("english","greek","hebrew"),
                        several.ok = FALSE)

  testament <- match.arg(testament,
                         c("old","new","both"),
                         several.ok = FALSE)

  # if chapter is null, verse should be null, too
  if(is.null(chapter) && !(is.null(verse))){
    stop("You must provide a valid chapter along with a matching selection of verses")
  }

  if(!is.null(chapter) && is.null(verses)){
    # group and filter by book and return
    # just the chapters specified
    # based on the language
     if(language == "english"){

     } else if(language == "greek"){

     } else {

     }
  } else {

  }

  # first filter the data by book,
  # then select the text based on
  # chapter, and verse selection.


  # output should just be the
  # text(character string),
  # not a data frame
}

