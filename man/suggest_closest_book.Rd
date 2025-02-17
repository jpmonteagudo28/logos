% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{suggest_closest_book}
\alias{suggest_closest_book}
\title{Suggest the closest matching book name}
\usage{
suggest_closest_book(book, book_list)
}
\arguments{
\item{book}{A character vector of book names to check.}

\item{book_list}{A character vector of valid book names to match against.}
}
\value{
A character vector of suggested book names or NA if no close match is found.
}
\description{
This function suggests the closest matching book name based on the input string.
It uses approximate string matching to find similar book names from the provided list.
}
\keyword{internal}
