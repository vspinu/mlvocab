context("vocab")
corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"), 
               b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
                     "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))

test_that("vocab is computed correctly", {

  v <- vocab(corpus) %>% .[order(.[["term_count"]]), ]
  vt <- structure(
    list(term = c("The", "quick", "brown", "fox", "jumps", "over", "lazy", "dog", "the"),
         term_count = c(1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 5L),
         doc_count = c(1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L)), 
    ngram = c(1L, 1),
    document_count = 2L,
    nbuckets = 0L, 
    ngram_sep = "_",
    row.names = c(1L, 2L, 3L, 4L, 5L, 6L, 8L, 9L, 7L),
    class = c("mlvocab_vocab", "data.frame"))
  expect_equal(v, vt)

  v <- vocab(corpus, ngram = c(2, 3), ngram_sep = " ") %>% .[order(.[["term_count"]]), ]
  vt <- structure(
    list(term = c("The quick", "The quick brown", "lazy dog the", 
                  "dog the", "dog the quick", "the quick", "the quick brown", "quick brown", 
                  "quick brown fox", "brown fox", "brown fox jumps", "fox jumps", 
                  "fox jumps over", "jumps over", "jumps over the", "over the", 
                  "over the lazy", "the lazy", "the lazy dog", "lazy dog"),
         term_count = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
         doc_count = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L)), 
    ngram = c(2L, 3L), 
    document_count = 2L,
    nbuckets = 0L, 
    ngram_sep = " ",
    row.names = c(1L, 2L, 18L, 19L, 20L, 16L, 17L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L), 
    class = c("mlvocab_vocab", "data.frame"))
  expect_equal(v, vt)
  
})

test_that("vocab adds new terms to the end", {
  v <- vocab(corpus)
  extras <- list(extras = c("apples", "oranges"))
  v2 <- vocab(c(corpus, extras))
  expect_equal(v2$term[-c(1:nrow(v))], extras$extras)
  expect_equal(v2, vocab_update(v, extras))
})

test_that("vocab_prune works as expected", {

  v <- vocab(corpus)
  expect_equal(vocab_prune(v, max_terms = 8)$term,
               c("quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
  expect_equal(vocab_prune(v, term_count_min = 2)$term,
               c("quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
  expect_equal(vocab_prune(v, term_count_max = 3)$term,
               c("The", "quick", "brown", "fox", "jumps", "over", "lazy", "dog"))

})


test_that("vocab_prune adds buckets correctly", {

  v <- vocab(corpus)
  vb <- vocab_prune(v, max_terms = 2, nbuckets = 0)
  expect_equal(attr(vb, "nbuckets"), 0)
  expect_true(all(colSums(v[, 2:3]) >  colSums(vb[, 2:3])))
  expect_equal(nrow(vb), 2)
  expect_true("the" %in% vb$term)

  v <- vocab(corpus)
  vb <- vocab_prune(v, max_terms = 2, nbuckets = 3)
  expect_equal(attr(vb, "nbuckets"), 3)
  expect_equal(colSums(v[, 2:3]), colSums(vb[, 2:3]))
  expect_equal(nrow(vb), 5)
  expect_true("the" %in% vb$term)

  v <- vocab(corpus, c(1, 2))
  vb <- vocab_prune(v, max_terms = 10, nbuckets = 3)
  expect_equal(attr(vb, "nbuckets"), 3)
  expect_equal(colSums(v[, 2:3]), colSums(vb[, 2:3]))
  expect_equal(nrow(vb), 13)
  expect_true("the" %in% vb$term)

})

test_that("vocab_update fails on pruned vocabularies", {
  v <- vocab(corpus, c(1, 2))
  v <- vocab_prune(v, max_terms = 10, nbuckets = 3)
  expect_error(vocab_update(v, corpus))
})

test_that("vocab_prune puts unknown buckets at the end", {
  v <- vocab(corpus, c(1, 2))
  v10 <- vocab_prune(v, max_terms = 10, nbuckets = 3)
  v2a <- vocab_prune(v10[sample(nrow(v10), nrow(v10)), ], max_terms = 1)
  v2b <- vocab_prune(v10, max_terms = 1)
  expect_equal(v2a, v2b)
})

test_that("vocab_prune works incrementally", {

  v <- vocab(corpus, c(1, 2))
  vb2 <- vocab_prune(v, max_terms = 2, nbuckets = 3)
  vb10 <- vocab_prune(v, max_terms = 10, nbuckets = 3)

  expect_error(vocab_prune(vb10, max_terms = 3, nbuckets = 2))

  vb10_2 <- vocab_prune(vb10, max_terms = 2, nbuckets = 3)
  expect_equal(vb2, vb10_2)

  tvb <- vocab_prune(vb10, doc_count_min = 3)
  expect_equal(nrow(tvb), 3)
  expect_equal(colSums(tvb[, 2:3]), colSums(v[, 2:3]))

  tvb10 <- vocab_prune(v, max_terms = 10, nbuckets = 0)
  tvb2 <- vocab_prune(tvb10, max_terms = 2, nbuckets = 3)
  expect_equal(nrow(tvb2), 5)
  expect_equal(colSums(tvb10[, 2:3]), colSums(tvb2[, 2:3]))

})


test_that("encodding doesn't matter", {

  txt <- c("”", "“", "–", "’", "…", "—", "‘", "•", "»", 
           "·", "�", "£", "«", "→", "®", "🙂", "←", "€", "™", 
           "©", "﻿", "­", "​", "−", "\u0093", "\u0094", "›", "\u0097", 
           "×", "§")

  v1 <- vocab(txt)
  Encoding(txt) <- "UTF-8"
  v2 <- vocab(txt)
  Encoding(txt[1:5]) <- "native"
  v3 <- vocab(txt)

  expect_equal(Encoding(v1$term), Encoding(v2$term))
  expect_equal(Encoding(v1$term), Encoding(v3$term))

})
