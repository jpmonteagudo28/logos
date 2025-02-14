rasb_bible_books <- unique(rasb_bible$book)
nt_books <- unique(new_testament$book)
ot_books <- unique(old_testament$book)

gen_fraction <- c(
                  "In the beginning God created the heavens and the earth.",
                  "And the earth was waste and void. And darkness was upon the face of the deep. And the Spirit of God moved upon the face of the waters.",
                  "And God said, Let there be light. And there was light."
                  )

jude <- c(
           "Ἰούδας Ἰησοῦ Χριστοῦ δοῦλος, ἀδελφὸς δὲ Ἰακώβου, τοῖς ἐν θεῷ πατρὶ ⸀ἠγαπημένοις καὶ Ἰησοῦ Χριστῷ τετηρημένοις κλητοῖς· "  ,
           "ἔλεος ὑμῖν καὶ εἰρήνη καὶ ἀγάπη πληθυνθείη. " ,
           "Ἀγαπητοί, πᾶσαν σπουδὴν ποιούμενος γράφειν ὑμῖν περὶ τῆς κοινῆς ⸀ἡμῶν σωτηρίας ἀνάγκην ἔσχον γράψαι ὑμῖν παρακαλῶν ἐπαγωνίζεσθαι τῇ ἅπαξ παραδοθείσῃ τοῖς ἁγίοις πίστει. "
         )

# Tests for get_bible_version
test_that("get_bible_version correctly filters Old Testament", {
  expect_equal(
    unique(get_bible_version("English", "Old Testament")$book),
    rasb_bible_books[1:39]
  )
})

test_that("get_bible_version correctly filters New Testament", {
  expect_equal(
    unique(get_bible_version("English", "New Testament")$book),
    rasb_bible_books[40:66]
  )
})

test_that("get_bible_version errors for incorrect testament", {
  expect_null(get_bible_version("English", "Invalid"), NULL)
})

test_that("get_bible_version errors for incorrect language", {
  expect_error(get_bible_version("Latin", "Old Testament"), "'arg' should be one of \"English\", \"Hebrew\", \"Greek\"")
})

test_that("get_bible_version errors when Hebrew selected for New Testament", {
  expect_error(get_bible_version("Hebrew", "New Testament"), "The Leningrad Codex only includes the Old Testament.")
})

test_that("get_bible_version errors when Greek selected for Old Testament", {
  expect_error(get_bible_version("Greek", "Old Testament"), "The Greek New Testament only includes the New Testament.")
})

# Tests for get_fraction
test_that("get_fraction extracts the correct section", {
  result <- get_fraction("Gen", chapter = 1, fraction = 12,part = 1, language = "English",testament = "old")
  expect_equal(result,gen_fraction)
})

test_that("get_fraction errors for invalid fraction and part", {
  expect_error(get_fraction("Jud", 1, 0, 1, language = "English",testament = "new"), "Invalid fraction or part. Ensure that fraction >= 1 and 1 <= part <= fraction.")
  expect_error(get_fraction("Genesis", 1, 3, 4, language = "Hebrew",testament = "old"), "Invalid fraction or part. Ensure that fraction >= 1 and 1 <= part <= fraction.")
})

test_that("get_fraction correctly handles small chapters", {
  result <- get_fraction("Jud", 1, 10, 1, language = "Greek",testament = "new")
  expect_equal(result, jude)
})
