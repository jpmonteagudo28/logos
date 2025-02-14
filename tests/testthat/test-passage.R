
test_that("select_passage works with standard book, chapter, and verse input", {
  expect_equal(
    select_passage(book = "Genesis", chapter = 1, verse = 1,language = "English",testament = "old"),
    "In the beginning God created the heavens and the earth."
  )
})

test_that("select_passage enforces valid testament values", {
  expect_error(select_passage(testament = "Invalid"),
               "arg' should be one of \"Old\", \"New\", \"Both\", \"old\", \"new\", \"both\"")
})

test_that("select_passage enforces valid language values", {
  expect_error(select_passage(language = "Latin", testament = "new"),
               "'arg' should be one of \"English\", \"Hebrew\", \"Greek\"")
})

test_that("select_passage enforces numeric chapter and verse", {
  expect_error(select_passage(chapter = "One", language = "English",testament = "new"), "chapter must be a numeric vector.")
  expect_error(select_passage(chapter = 1, verse = "One",language = "English",testament = "new"), "verse must be a numeric vector.")
})

test_that("select_passage enforces correct 'by' usage", {
  expect_error(select_passage(book = "Genesis", by = "author",language = "English",testament = "new"),
               "If selecting by author, section, or date, the 'book', 'chapter', and 'verse' arguments must be NULL.")
  expect_error(select_passage(by = "invalid",language = "English",testament = "new"),
               "'arg' should be one of \"author\", \"section\", \"date\"")
})

test_that("select_passage requires divider when using 'by'", {
  expect_error(select_passage(by = "author",language = "English",testament = "new"),
               "A 'divider' must be provided when selecting by criteria.")
})

test_that("select_passage errors on invalid author, section, or date", {
  expect_error(select_passage(by = "author", divider = "Ernest",language = "English",testament = "new"),
               "Invalid author provided in 'divider'. Please check 'author_data' for valid options.")
  expect_error(select_passage(by = "section", divider = "Unknown",language = "English",testament = "new"),
               "Invalid section provided in 'divider'. Please check 'author_data' for valid options.")
  expect_error(select_passage(by = "date", divider = "Some day",language = "English",testament = "new"),
               "Invalid date provided in 'divider'. Please check 'author_data' for valid options.")
})
