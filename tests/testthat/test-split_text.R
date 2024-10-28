test_that("split_text splits text correctly", {
  # Test with default width
  long_text <- "This is a long string that needs to be split into multiple lines."
  result <- split_text(long_text)
  expected <- "This is a long string that\nneeds to be split into\nmultiple lines."
  expect_equal(result, expected)
  
  # Test with custom width
  result <- split_text(long_text, width = 20)
  expected <- "This is a long string\nthat needs to be split\ninto multiple lines."
  expect_equal(result, expected)
  
  # Test with single word longer than width
  long_word <- "supercalifragilisticexpialidocious"
  result <- split_text(long_word, width = 10)
  expected <- long_word  # Should remain the same as it's a single long word
  expect_equal(result, expected)
  
  # Test with empty string
  result <- split_text("", width = 10)
  expected <- ""
  expect_equal(result, expected)
})
