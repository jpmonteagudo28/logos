test_that("Negation of %in% works correctly", {
  expect_true(1 %!in% c(2, 3, 4))
  expect_false(2 %!in% c(1, 2, 3))
})

test_that("%||% returns the left-hand value if it is not NULL", {
  expect_equal("Hello" %||% "World", "Hello")
  expect_equal(42 %||% 100, 42)
  expect_equal(TRUE %||% FALSE, TRUE)
})

test_that("replace_char correctly replaces characters", {
  expect_equal(replace_char("hello world", "world", "there"), "hello there")
  expect_equal(replace_char("hello world", "o", "0"), "hell0 w0rld")
  expect_error(replace_char("hello", "", "x"), "Error: The character to replace cannot be empty.")
})

test_that("first_to_upper capitalizes the first letter of each word", {
  expect_equal(unname(first_to_upper("hello world")), "Hello World")
  expect_equal(unname(first_to_upper("test case")), "Test Case")
  expect_equal(unname(first_to_upper("multiple   spaces")), "Multiple   Spaces")
})

test_that("first_to_lower converts the first letter of each word to lowercase", {
  expect_equal(unname(first_to_lower("Hello World")), "hello world")
  expect_equal(unname(first_to_lower("Test Case")), "test case")
  expect_equal(unname(first_to_lower("Multiple   Spaces")), "multiple   spaces")
})

test_that("to_char correctly converts symbols to character", {
  expect_equal(to_char(x), "x")
  expect_equal(to_char(hello), "hello")
  expect_equal(to_char(`test_var`), "test_var")
})

test_that("is_nested correctly identifies nested lists", {
  expect_false(is_nested(1))
  expect_false(is_nested(list(1, 2, 3)))
  expect_true(is_nested(list(1, list(2, 3))))
  expect_true(is_nested(list(list(1), list(2))))
})

test_that("is_empty correctly identifies empty inputs", {
  expect_true(is_empty(character(0)))
  expect_true(is_empty(list()))
  expect_false(is_empty(1))
  expect_false(is_empty(list(1, 2)))
})
