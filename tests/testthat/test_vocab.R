context("vocab")

corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the",
                     "lazy", "dog"), 
               b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                     "dog", "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                     "dog"))

test_that("vocab is computed correctly", {

    v <- vocab(corpus) %>% .[order(.[["term_count"]]), ]
    vt <- structure(
        list(term = c("The", "quick", "brown", "fox", "jumps", "over", "lazy", "dog", "the"),
             term_count = c(1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 5L),
             doc_count = c(1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L)), 
        ngram = c(1L, 1),
        document_count = 2L,
        sep_ngram = " ",
        chargram = FALSE,
        row.names = c(1L, 2L, 3L, 4L, 5L, 6L, 8L, 9L, 7L),
        class = c("mlvocab_vocab", "text2vec_vocabulary", "data.frame"))
    expect_equal(v, vt)

    v <- vocab(corpus, ngram = c(2, 3)) %>% .[order(.[["term_count"]]), ]
    vt <- structure(
        list(term = c("The quick", "The quick brown", "lazy dog the", 
                      "dog the", "dog the quick", "the quick", "the quick brown", "quick brown", 
                      "quick brown fox", "brown fox", "brown fox jumps", "fox jumps", 
                      "fox jumps over", "jumps over", "jumps over the", "over the", 
                      "over the lazy", "the lazy", "the lazy dog", "lazy dog"),
             term_count = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
             doc_count = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L)), 
        ngram = c(2L, 3L), 
        document_count = 2L, sep_ngram = " ", chargram = FALSE,
        row.names = c(1L, 2L, 18L, 19L, 20L, 16L, 17L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L), 
        class = c("mlvocab_vocab", "text2vec_vocabulary", "data.frame"))
    expect_equal(v, vt)
    
})

test_that("vocab adds new terms to the end", {
    v <- vocab(corpus)
    extras <- list(extras = c("apples", "oranges"))
    v2 <- vocab(c(corpus, extras))
    expect_equal(v2$term[-c(1:nrow(v))], extras$extras)
    expect_equal(v2, update_vocab(v, extras))
})

test_that("update_vocab works on text2vec_vocabulary", {
    v <- vocab(corpus)
    oldClass(v) <- c("text2vec_vocabulary", "data.frame")
    v3 <- update_vocab(v, list(tt = "tmp"))
    oldClass(v) <- c("mlvocab_vocab", "data.frame")
    v4 <- update_vocab(v, list(tt = "tmp"))
    expect_equal(v3, v4)
})

test_that("prune_vocab works as expected", {
    v <- vocab(corpus)
    expect_equal(prune_vocab(v, max_terms = 8)$term,
                 c("quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
    expect_equal(prune_vocab(v, term_count_min = 2)$term,
                 c("quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
    expect_equal(prune_vocab(v, term_count_max = 3)$term,
                 c("The", "quick", "brown", "fox", "jumps", "over", "lazy", "dog"))
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
