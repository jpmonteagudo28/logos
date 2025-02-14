old_moses <- matrix(c("Gen","Exod","Lev","Num","Deut"),
                    nrow = 5, ncol = 1,
                    dimnames =  list(NULL,"books"))

new_paul <- matrix(c("Rom", "1Cor" ,"2Cor", "Gal", "Eph",
              "Phil", "Col", "1Thess","2Thess",
              "1Tim", "2Tim", "Titus", "Phlm" ),
              nrow = 13, ncol = 1,
              dimnames = list(NULL,"books"))

new_dates <- data.frame(
  books = c("Matthew", "Mark", "Luke", "Romans", "1 Corinthians", "2 Corinthians",
            "Galatians", "Ephesians", "Philippians", "Colossians", "1 Thessalonians",
            "2 Thessalonians", "Philemon"),
  stringsAsFactors = FALSE
)

old_dates <- data.frame(
  books = c("Genesis", "Exodus", "Leviticus", "Numbers", "Deuteronomy",
            "Joshua", "Psalms"),
  stringsAsFactors = FALSE
)



test_that("by_author filters books correctly", {

  expect_equal(by_author("Moses", "Old Testament"), old_moses)
  expect_equal(by_author("Paul", "New Testament"), new_paul)
  expect_error(by_author("Mathias", "New Testament"),
               "The section selected does not match any of the sections contained in the 'author_data' dataset. Please, refer to this dataset for a complete list of sections")
})


test_that("by_date retrieves books within correct range", {

  expect_equal(by_date("1445 - 1405 BC", "Old Testament"), old_dates)
  expect_equal(by_date("50 - 60 AD", "New Testament"), new_dates)

  captured_warning <- capture_warnings(by_date("200 - 300 AD", "New Testament"))
  print(captured_warning)  # Check the actual captured warning
  expect_true(any(grepl("The date is not compatible", captured_warning)))
})
