% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/passage.R
\name{select_passage}
\alias{select_passage}
\title{Select a book, passage or portion of a book from the old or new testament}
\usage{
select_passage(
  book = NULL,
  chapter = NULL,
  verse = NULL,
  fraction = 1,
  part = 1,
  by = NULL,
  divider = NULL,
  language = "English",
  testament = NULL
)
}
\arguments{
\item{book}{A character vector specifying the book name(s) from which to select the passage.
Ignored if the \code{by} parameter is provided.}

\item{chapter}{A numeric vector indicating the chapter(s) of the book. If selection by section, author or date is used, the chapter must be set to NULL.}

\item{verse}{A numeric vector indicating the verse(s) within the chapter.}

\item{fraction}{A numeric value (default \code{1}) indicating how many equal parts to divide the chapter into.}

\item{part}{A numeric value specifying which part to return (must be between 1 and \code{fraction}).}

\item{by}{An optional character string for alternative selection criteria. It should be one of
\code{"author"}, \code{"section"}, or \code{"date"}. When provided, the \code{book}, \code{chapter},
and \code{verse} parameters must be \code{NULL}.}

\item{divider}{A character vector specifying the author, section or date range to be selected.}

\item{language}{A character string indicating the language of the passage. Must be one of
\code{"English"}, \code{"Hebrew"}, or \code{"Greek"}. Default is \code{"English"}.}

\item{testament}{A character string specifying the testament. Must be one of
\code{"Old"}, \code{"New"}, or \code{"Both"} (case-insensitive). This parameter is required.}
}
\value{
Returns the selected passage as generated by the \code{retrieve_chapter()} function.
}
\description{
Retrieves a whole book(s), passage from a book or a portion of a book or chapter, based on specified criteria such as book, chapter,
verse, or alternative selection using author, section, or date.
}
\details{
This function validates the input parameters and determines the book(s) to be used based on the provided criteria.
If the \code{by} argument is given, the function leverages helper functions such as \code{by_author()},
\code{by_section()}, or \code{by_date()} to select the appropriate book(s) based on the alternative criterion.
The passage is then retrieved via \code{retrieve_chapter()}, applying verse filtering and partitioning based on
the \code{fraction} and \code{part} arguments.
}
\examples{
# Example 1: Select a passage by specifying book, chapter, and verse.
select_passage(book = "Genesis", chapter = 1, verse = 1, testament = "Old")
select_passage(book = "Mat", chapter = 1, verse = 1:10, testament = "new")
}
