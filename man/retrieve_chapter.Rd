% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieve.R
\name{retrieve_chapter}
\alias{retrieve_chapter}
\title{Retrieve a specific chapter or portion of a chapter from the Bible}
\usage{
retrieve_chapter(
  book,
  chapter = NULL,
  verse = NULL,
  fraction = NULL,
  part = NULL,
  language,
  testament
)
}
\arguments{
\item{book}{A character string specifying the book of the Bible.}

\item{chapter}{A numeric or character vector specifying the chapter(s) to retrieve.}

\item{verse}{An optional numeric vector specifying specific verses to retrieve.}

\item{fraction}{A numeric value indicating how many equal parts to divide the chapter into.}

\item{part}{A numeric value specifying which part to return (must be between 1 and \code{fraction}).}

\item{language}{A character string specifying the language of the Bible text. Options are "English", "Hebrew", or "Greek".}

\item{testament}{A character string specifying whether to retrieve from the Old or New Testament.}
}
\value{
A character vector containing the retrieved Bible text.
}
\description{
This function retrieves text from a specified book and chapter, with optional filtering
by verse, partitioning, and language selection.
}
\examples{
retrieve_chapter("Jud",chapter = 1, verse = 1, language = "English", testament = "new")

}
