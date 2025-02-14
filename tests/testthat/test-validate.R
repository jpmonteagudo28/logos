test_that("suggest_closest_book correctly suggests closest match", {
  book_list <- c("Genesis", "Exodus", "Leviticus", "Numbers", "Deuteronomy")

  expect_equal(suggest_closest_book("Gen", book_list), "Genesis")
  expect_equal(suggest_closest_book("Exo", book_list), "Exodus")
  expect_true(is.na(suggest_closest_book("UnknownBook", book_list)))
})

# Test for validate_book

test_that("validate_book correctly validates and suggests books", {
  book_list <- c("Gen", "Exod", "Lev", "Num", "Deut")

  expect_equal(validate_book("Genesis", book_list), "Gen")
  expect_equal(validate_book("Leviticus", book_list), "Lev")
  expect_true(is.na(validate_book("RandomBook", book_list)))
})

# Test for standardize_name

test_that("standardize_name correctly converts book names to standard format", {
  expect_equal(standardize_name("Genesis"), "Gen")
  expect_equal(standardize_name("Exo"), "Exod")
  expect_equal(standardize_name("Psalms"), "Ps")
  expect_equal(standardize_name("1 Corinthians"), "1Cor")
  expect_equal(standardize_name("UnknownBook"),"UnknownBook")
})
