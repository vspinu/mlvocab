context("ngram")

test_that("ngram weights are consistent", {
  
  w11 <- mlvocab:::ngram_weights(ngram_min = 1, ngram_max = 1)
  w12 <- mlvocab:::ngram_weights(ngram_min = 1, ngram_max = 2)
  w23 <- mlvocab:::ngram_weights(ngram_min = 2, ngram_max = 3)
  w13 <- mlvocab:::ngram_weights(ngram_min = 1, ngram_max = 3)

  expect_equal(w13[names(w11)], w11)
  expect_equal(w13[names(w12)], w12)
  expect_equal(w13[names(w23)], w23)

})

