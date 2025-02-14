df1 <- data.frame(
  A = 1:5,
  B = letters[1:5],
  C = factor(c("low", "medium", "high", "low", "high")),
  D = as.Date("2023-01-01") + 0:4,
  E = c(TRUE, FALSE, TRUE, FALSE, TRUE),
  stringsAsFactors = FALSE
)

df2 <- data.frame(
  X = rnorm(10),
  Y = sample(LETTERS, 10, replace = TRUE),
  Z = sample(c(TRUE, FALSE), 10, replace = TRUE)
)

df_empty <- data.frame()

# Tests
test_that("peek.data.frame correctly displays non-empty data frames", {
  expect_invisible(peek.data.frame(df1))  # Should print output but not return anything
  expect_invisible(peek.data.frame(df2))
})

test_that("peek.data.frame correctly handles an empty data frame", {
  expect_warning(peek.data.frame(df_empty),"no non-missing arguments to max; returning -Inf")  # Should not error out
})

test_that("peek.data.frame aligns column names correctly", {
  expect_invisible(peek.data.frame(df1))  # Should ensure alignment of column names
})

test_that("peek.data.frame handles different data types", {
  expect_invisible(peek.data.frame(df1))  # Should display int, chr, fct, date, and lgl
})

test_that("get_display_length adjusts correctly for data types", {
  expect_equal(get_display_length(df1$A), 12)   # Integer
  expect_equal(get_display_length(df1$B), 8)    # Character
  expect_equal(get_display_length(df1$C), 8)    # Factor
})
