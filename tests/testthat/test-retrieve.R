
# Run tests
test_that("retrieve_chapter correctly retrieves full chapter", {
  result <- retrieve_chapter(book = "Genesis", chapter = 1, verse =1,language = "English", testament = "Old")
  expect_type(result, "character")
  expect_length(result, 1)
  expect_equal(result, "In the beginning God created the heavens and the earth.")
})

test_that("retrieve_chapter errors on missing book", {
  expect_error(retrieve_chapter(book = "Nonexistent", chapter = 1, language = "English", testament = "Old"),
               "No matching verses found for the given book and chapter.")
})

test_that("retrieve_chapter retrieves fraction of a chapter", {
  result <- retrieve_chapter(book = "Genesis", chapter = 1, verse = 1:2,fraction = 2, part = 1, language = "English", testament = "Old")
  expect_type(result, "character")
  expect_length(result, 1)  # Only one verse in mock data
})

test_that("retrieve_chapter errors on invalid fraction/part", {
  expect_error(retrieve_chapter(book = "Genesis", chapter = 1, fraction = 2, part = 3, language = "English", testament = "Old"),
               "Invalid fraction or part. Ensure that fraction >= 1 and 1 <= part <= fraction.")
})
