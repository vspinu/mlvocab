context("tokenizer")

test_that("Tokenizer tokenizes with ASCII tokens", {
  expect_equal(C_tokenize(c("fsfsabcdsssfdsffsfsd", "abcdssfdsffsfsd"), "sf"),
               list(c("abcd", "d", "d"), c("abcd", "d", "d")))  
})
