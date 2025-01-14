#' RASB Bible
#'
#' A public domain dataset containing the text of the Bible from the Revised American Standard Bible (RASB).
#'
#' @format A data frame with 31,102 rows and 4 variables:
#' \describe{
#'   \item{book}{Character. Name of the book (e.g., "Gen").}
#'   \item{chapter}{Integer. Chapter number.}
#'   \item{verse}{Integer. Verse number.}
#'   \item{text}{Character. Text of the verse.}
#' }
#' @details This dataset provides the full English text of the Bible from the RASB translation.
"rasb_bible"

#' The Society of Biblical Literature Greek New Testament
#'
#' A dataset containing the text of the New Testament in Greek with book metadata.
#' The Society of Biblical Literature, in keeping with its mission to foster
#' biblical scholarship, is pleased to sponsor, in association with Logos Bible
#' Software, a new, critically edited edition of the Greek New Testament.
#'
#' @format A data frame with 7,939 rows and 5 variables:
#' \describe{
#'   \item{book}{Character. Name of the book (e.g., "1Cor").}
#'   \item{chapter}{Character. Chapter number.}
#'   \item{verse}{Character. Verse number.}
#'   \item{text}{Character. Text of the verse in Greek.}
#'   \item{greek_name}{Character. Greek name of the book.}
#' }
#' @details This dataset provides the Greek text of the New Testament along with book and chapter metadata.
"new_testament"

#' Old Testament Dataset
#'
#' A dataset containing the text of the Old Testament in Hebrew with book metadata.
#' The Unicode/XML Leningrad Codex (UXLC) is a transcription of the Leningrad Codex (LC) into a
#' modern computer format (Unicode, XML). The UXLC text is a fork of the Groves Center's Westminster
#'  Leningrad Codex [ WLC 4.20, 2016](https://www.bible.com/versions/904-wlc-westminster-leningrad-codex-groves-center-version)
#'
#' @format A data frame with 23,213 rows and 5 variables:
#' \describe{
#'   \item{book}{Character. Name of the book (e.g., "Amos").}
#'   \item{chapter}{Numeric. Chapter number.}
#'   \item{verse}{Numeric. Verse number.}
#'   \item{text}{Character. Text of the verse in Hebrew.}
#'   \item{hebrew_names}{Character. Hebrew name of the book.}
#' }
#' @details This dataset provides the Hebrew text of the Old Testament along with book and chapter metadata.
"old_testament"

#' Author Data for Biblical Books
#'
#' A dataset containing information about the authors, books, sections, and languages of the Bible.
#'
#' @format A data frame with 66 rows and 6 variables:
#' \describe{
#'   \item{author}{Character. Name of the author (e.g., "Moses").}
#'   \item{books}{Character. Name of the book(s) authored.}
#'   \item{section}{Character. Section of the Bible (e.g., "Law", "History").}
#'   \item{date}{Character. Approximate date of authorship (e.g., "c. 1445-1405 BC").}
#'   \item{testament}{Character. Testament classification ("Old Testament" or "New Testament").}
#'   \item{language}{Character. Language in which the book was originally written (e.g., "Hebrew").}
#' }
#' @details This dataset summarizes metadata about biblical authors, sections, and book classifications.
"author_data"
