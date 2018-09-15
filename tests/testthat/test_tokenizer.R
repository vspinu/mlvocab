context("tokenizer")

## C_tokenize(c("fsfsabcdsssfdsffsfsd", "abcdssfdsffsfsd"), "[sf]")
## stringi::stri_split_regex(c("fsfsabcdsssfdsffsfsd", "abcdssfdsffsfsd"), "[sf]")
## C_tokenize(c("aℵζsdfd γκkkdaβδbdc αbbf"), "[ α-ϵ]+")
## C_tokenize(c("aℵb aβb α β", "abcdssfdsffsfsd"), "[ b β]+")

test_that("NULL separator results in no segmentation", {
  corpus <- c("some sentence", "another sentence")
  v <- vocab(corpus, regex = NULL)
  expect_equal(v$term, corpus)
  expect_equal(v, vocab(corpus, regex = ""))
})

test_that("Tokenizer tokenizes with ASCII tokens", {
  expect_equal(C_tokenize(c("fsfsabcdsssfdsffsfsd", "abcdssfdsffsfsd"), "[sf]+"),
               list(c("abcd", "d", "d"), c("abcd", "d", "d")))
})

test_that("Tokenizer tokenizes with Unicode tokens", {
  expect_equal(C_tokenize(c("aℵb aβb α β", "abcdssfdsffsfsd"), "[b ]+"),
               list(c("aℵ", "aβ", "α", "β"), c("a", "cdssfdsffsfsd")))
})

test_that("utf8 string corpus is correctly tokenized with space", {
  corpus <- c("a α b β", "aℵb aβb α β", "ASCII string")
  v <- vocab(corpus, regex = " ")
  expect_equal(v$term, c("α", "β", "a",  "b", "aℵb", "aβb", "ASCII", "string"))
})

test_that("Tokenizer tokenizes with Unicode tokens and Unicode regex", {
  skip_on_os("windows")
  expect_equal(C_tokenize(c("aℵb aβb α β", "abcdssfdsffsfsd"), "[ b β]+"),
               list(c("aℵ", "a", "α"), c("a", "cdssfdsffsfsd")))
})

test_that("Tokenizer handles missing values with Unicode regex", {
  skip_on_os("windows")
  expect_equal(C_tokenize(c(NA, "aℵb aβb α β", NA, "abcdssfdsffsfsd"), "[ b β]+"),
               list(NA_character_, c("aℵ", "a", "α"), NA_character_,  c("a", "cdssfdsffsfsd")))
})

test_that("utf8 string corpus is correctly tokenized with Unicode regex", {
  skip_on_os("windows")
  corpus <- c("a α b β", "aℵb aβb α β", "ASCII string")
  v <- vocab(corpus, regex = "[ abβ]")
  expect_equal(v$term, c("α", "ℵ", "ASCII", "string"))
  ## str(vocab(c(NA, "aℵb aβb α β", NA, "abcdssfdsffsfsd"), regex = "[ b β]"))
})

test_that("Tokenizer tokenizes with Unicode range regexp", {
  skip_on_os("windows")
  expect_equal(C_tokenize(c("aℵζsdfd γκkkdaβδbdc γδϵbbf"), "[ α-ϵ]+"),
               list(c("aℵ", "sdfd", "kkda", "bdc", "bbf")))
})
