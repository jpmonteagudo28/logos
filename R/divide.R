divide_by <- function(book = NULL,
                      chapter = NULL,
                      verse = NULL,
                      author = NULL,
                      section = NULL, # gospel, law, etc.
                      date_range = NULL, # date range
                      testament = NULL, # old and new
                      language = NULL){ # heb, greek, eng

  language <- match.arg(language,
                        c("english","greek",
                          "hebrew"),
                        several.ok = FALSE)

  testament <- match.arg(testament,
                         c("old","new","both"),
                         several.ok = FALSE)

  # if chapter is null,
  # verse should be null, too
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


  if(!is.null(author)){
    default <- author_data$author

    if(author %!in% default){
      stop("'author' not found in list of biblical authors. To find a list of authors
           refer to the 'author_data'dataset.")
    } else {
      author <- author
    }
  }

  if(!is.null(section)){
    default_section <- author_data$section

    if(section %!in% default_section){
      stop("'section' not found in list of biblical sections. To find a list of
           valid sections refer to the 'author_data' dataset.")
    } else {

    }
  }

  if(!is.null(date_range)){
    default_date <- author_data$date

    if(testament == "new"){
      regex <- "\\b(c\\.\\s*\\d{1,2}(?:-\\d{1,2})?\\s*(AD)|Unknown)\\b"
      matches <- grep(regex, default_date, value = TRUE)
    }
    if(testament == "old"){
      regex <- "\\bc\\.\\s*(\\d{3,4}(?:-\\d{3,4})?)\\s*(BC)|Unknown\\b"
      matches <- grep(regex, default_date, value = TRUE)
    }


  }


  # first filter the data by book,
  # then select the text based on
  # chapter, and verse selection.

}
