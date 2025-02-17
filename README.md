
<!-- README.md is generated from README.Rmd. Please edit that file -->

# logos <img src="man/figures/logo.png" align="right" height="275" alt="" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/logos)](https://CRAN.R-project.org/package=logos)
![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![](https://codecov.io/gh/jpmonteagudo28/logos/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jpmonteagudo28/logos)
![Static
Badge](https://img.shields.io/badge/biblical-research-%23035949)
[![](https://cranlogs.r-pkg.org/badges/logos)](https://cran.r-project.org/package=logos)
<!-- badges: end -->

The goal of `logos` is to give access to the Greek New Testament (27
books) and the Tanach (39 books) and allow users to do textual analysis
on the data. The New and Old Testament have been provided in their
original languages, Greek and Hebrew, respectively. Additionally, the
~~English Standard Version (ESV)~~ *Revised American Standard Bible*
(RASB) is also provided for users who’d rather use a word–for–word
modern (1901) English translation.

## Installation

You can install the development version of `logos` like so:

``` r
devtools::install_github("jpmonteagudo28/logos")
```

or download the package from CRAN like so:

``` r
install.packages("logos")
```

## How to

The main function in this package is the `select_passage()` function as
it allows you to retrieve entire sections, books, chapters or verses of
the English Bible (RASB), Greek New Testament, and Hebrew Old Testament.
The remaining functions are helpers that facilitate retrieval and
manipulation of biblical text.

Besides `select_passage()`, you have access to five datasets. The
*Revised Standard American Bible*, ‘rasb_bible’, the *Society of
Biblical Literature Greek New Testament*,‘new_testament’, and the
*Leningrad Codex* containing the Old testament,‘old_testament’. Two
additional datasets are ‘author_data’ which contains a breakdown of the
authors, dates, sections and books of the Old and New Testament, and
‘verses_by_book’, which provides a count of the total number of verses
for each book of the bible.

``` r
library(logos)

# Let's grab a passage from the Gospel of John, chapter 1, verses 1 - 6
select_passage("Jhn",chapter = 1, verse = 1:6, language = "English", testament = "new)

# You can do so it a different language by changing the language argument
select_passage("Jhn",chapter = 1, verse = 1:6, language = "Greek", testament = "new)

# Notice that if you provide incompatible language and testament combinations for the Greek and Hebrew text, the function will throw an error and remind you to use the right combinations. 
select_passage("Jhn",chapter = 1, verse = 1:6, language = "Hebrew", testament = "new)
```

A secondary function is `peek()`, a base R replacement for dplyr’s
`glimpse()`. This function can be used to quickly get a sense of the
data you’ll be working with.

``` r
peek(old_testament)

peek(verses_by_book)
```

## Some real-life examples

Statistics have been applied to the field of biblical research for some
years now, and the creation of the
[stylo](https://github.com/computationalstylistics/stylo) and
[quanteda](https://github.com/quanteda/quanteda) package have made it
much easier to perform qualitative quantitative analysis on the biblical
data.

I leave a few interesting articles that show the intermingling of stats
and scripture.

- [Name Recall in the Synoptic
  Gospels](https://www.cambridge.org/core/journals/new-testament-studies/article/name-recall-in-the-synoptic-gospels/9507AEED21DACD1E1AC096B5321699C5)

- [Data from: Critical biblical studies via word frequency analysis:
  Unveiling text
  authorship](https://research.repository.duke.edu/concern/datasets/05741s76w?locale=en)

## Project status

Actively developed, though the pace has slowed now that I’m busier with
other packages and my school work. I have no plans to substantially
enlarge or extend it before really testing it.

## Contributions

If you would like to contribute to this package, I’d love your help!
Please read the
[guidelines](https://logos.jpmonteagudo.com/CONTRIBUTING.html) for
submitting a pull request.

## Code of Conduct

Please note that the logos project is released with a [Contributor Code
of Conduct](https://logos.jpmonteagudo.com/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
