context("vocab")

corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"),
               b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
                     "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
nms <- unique(unlist(corpus))
scorpus <- sapply(corpus, paste, collapse = " ")
dcorpus <- data.frame(names = names(corpus))
dcorpus$corpus <- corpus
dscorpus <- data.frame(names = names(scorpus), corpus = unname(scorpus), stringsAsFactors = F)

test_that("vocab is computed correctly", {

  v <- vocab(corpus, regex = " ")
  vt <- structure(list(term = c("the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog", "The"),
                       term_count = c(5L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 1L),
                       doc_count = c(2L, 2L, 2L, 2L, 2L, 2L, 2L,  2L, 1L)),
                  row.names = c(NA, -9L), class = c("mlvocab_vocab", "data.frame"),
                  ngram = c(1L, 1L),
                  document_count = 2L, nbuckets = 0L, ngram_sep = "_", regex = " ")

  expect_equal(v, vt)

  v <- vocab(corpus, ngram = c(2, 3), ngram_sep = " ", regex = " ") %>% .[order(.[["term_count"]]), ]
  vt <- structure(list(term = c("dog the quick", "dog the", "The quick",
                                "lazy dog the", "The quick brown", "the quick brown", "the quick",
                                "quick brown", "quick brown fox", "brown fox", "brown fox jumps",
                                "fox jumps", "fox jumps over", "jumps over", "jumps over the",
                                "over the", "over the lazy", "the lazy", "the lazy dog", "lazy dog"
                                ),
                       term_count = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
                       doc_count = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L)),
                  ngram = 2:3, document_count = 2L, nbuckets = 0L, ngram_sep = " ", regex = " ",
                  row.names = c(16L, 17L, 18L, 19L, 20L, 14L, 15L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L),
                  class = c("mlvocab_vocab", "data.frame"
                                                                                                                                                                                                                                           ))
  expect_equal(v, vt)

})

test_that("vocab adds new terms to the end", {
  v <- vocab(corpus, regex=" ")
  sv <- vocab(scorpus, regex=" ")
  dv <- vocab(dcorpus, regex=" ")
  dsv <- vocab(dscorpus, regex=" ")
  expect_equal(v, sv)
  expect_equal(v, dv)
  expect_equal(v, dsv)
  extras <- list(extras = c("apples", "oranges"))
  v2 <- vocab(c(corpus, extras), regex = " ")
  expect_equal(v2$term[-c(1:nrow(v))], extras$extras)
  expect_equal(v2, update_vocab(v, extras))
  sv2 <- vocab(c(scorpus, paste(extras[[1]], collapse = "   ")), regex = " ")
  expect_equal(v2, sv2)
})

test_that("prune_vocab works as expected", {
  v <- vocab(corpus)
  sv <- vocab(scorpus, regex = " ")
  dv <- vocab(dcorpus)
  dsv <- vocab(dscorpus, regex = " ")
  expect_equal(prune_vocab(v, max_terms = 8)$term,
               prune_vocab(sv, max_terms = 8)$term)
  expect_equal(prune_vocab(v, max_terms = 8)$term,
               prune_vocab(dv, max_terms = 8)$term)
  expect_equal(prune_vocab(v, max_terms = 8)$term,
               prune_vocab(dsv, max_terms = 8)$term)
  expect_equal(prune_vocab(v, max_terms = 8)$term,
               c("the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog"))
  expect_equal(prune_vocab(v, term_count_min = 2)$term,
               c("the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog"))
  expect_equal(prune_vocab(v, term_count_max = 3)$term,
               c("quick", "brown", "fox", "jumps", "over", "lazy", "dog", "The"))
})


test_that("prune_vocab adds buckets correctly", {

  v <- vocab(corpus)
  vb <- prune_vocab(v, max_terms = 2, nbuckets = 0)
  expect_equal(attr(vb, "nbuckets"), 0)
  expect_true(all(colSums(v[, 2:3]) >  colSums(vb[, 2:3])))
  expect_equal(nrow(vb), 2)
  expect_true("the" %in% vb$term)

  v <- vocab(scorpus, regex = " ")
  vb <- prune_vocab(v, max_terms = 2, nbuckets = 3)
  expect_equal(attr(vb, "nbuckets"), 3)
  expect_equal(colSums(v[, 2:3]), colSums(vb[, 2:3]))
  expect_equal(nrow(vb), 5)
  expect_true("the" %in% vb$term)

  v <- vocab(scorpus, c(1, 2), regex = " ")
  vb <- prune_vocab(v, max_terms = 10, nbuckets = 3)
  expect_equal(attr(vb, "nbuckets"), 3)
  expect_equal(colSums(v[, 2:3]), colSums(vb[, 2:3]))
  expect_equal(nrow(vb), 13)
  expect_true("the" %in% vb$term)

})

test_that("update_vocab fails on pruned vocabularies", {
  v <- vocab(corpus, c(1, 2))
  v <- prune_vocab(v, max_terms = 10, nbuckets = 3)
  expect_error(update_vocab(v, corpus))
})

test_that("prune_vocab puts unknown buckets at the end", {
  v <- vocab(corpus, c(1, 2))
  v10 <- prune_vocab(v, max_terms = 10, nbuckets = 3)
  v2a <- prune_vocab(v10[sample(nrow(v10), nrow(v10)), ], max_terms = 1)
  v2b <- prune_vocab(v10, max_terms = 1)
  expect_equal(v2a, v2b)
})

test_that("prune_vocab works incrementally", {

  v <- vocab(corpus, c(1, 2))
  sv <- vocab(corpus, c(1, 2))
  vb2 <- prune_vocab(v, max_terms = 2, nbuckets = 3)
  vb10 <- prune_vocab(sv, max_terms = 10, nbuckets = 3)

  expect_error(prune_vocab(vb10, max_terms = 3, nbuckets = 2))

  vb10_2 <- prune_vocab(vb10, max_terms = 2, nbuckets = 3)
  expect_equal(vb2, vb10_2)

  tvb <- prune_vocab(vb10, doc_count_min = 3)
  expect_equal(nrow(tvb), 3)
  expect_equal(colSums(tvb[, 2:3]), colSums(v[, 2:3]))

  tvb10 <- prune_vocab(v, max_terms = 10, nbuckets = 0)
  tvb2 <- prune_vocab(tvb10, max_terms = 2, nbuckets = 3)
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
