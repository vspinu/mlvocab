context("tokenizer")

test_that("Tokenizer tokenizes with ASCII tokens", {
  expect_equal(C_tokenize(c("fsfsabcdsssfdsffsfsd", "abcdssfdsffsfsd"), "sf"),
               list(c("abcd", "d", "d"), c("abcd", "d", "d")))  
})

test_that("Tokenizer tokenizes with Unicode tokens", {
  expect_equal(C_tokenize(c("aℵb aβb α β", "abcdssfdsffsfsd"), " b"),
               list(c("aℵ", "aβ", "α", "β"), c("a", "cdssfdsffsfsd")))  
})

test_that("Tokenizer tokenizes with Unicode tokens and Unicode seps", {
  expect_equal(C_tokenize(c("aℵb aβb α β", "abcdssfdsffsfsd"), " bβ"),
               list(c("aℵ", "a", "α"), c("a", "cdssfdsffsfsd")))  
})

test_that("utf8 string corpus is correctly tokenized", {
  corpus <- c("a α b β", "ASCII string")
  v <- vocab(corpus, seps = " ")
  expect_equal(v$term, c("a", "α", "b", "β", "ASCII", "string"))
  v <- vocab(corpus, seps = "α ")
  expect_equal(v$term, c("a", "b", "β", "ASCII", "string"))
})
