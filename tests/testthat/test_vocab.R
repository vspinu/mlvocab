context("vocab")

corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the",
                     "lazy", "dog"), 
               b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                     "dog", "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                     "dog"))

test_that("vocab is computed correctly", {

    v <- vocab(corpus) %>% .[order(.[["term_count"]]), ]
    vt <- structure(
        list(
            term = c("The", "over", "dog", "fox", "brown",  "lazy", "jumps", "quick", "the"),
            term_count = c(1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 5L),
            doc_count = c(1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L)),
        ngram = c(1L, 1),
        document_count = 2L,
        sep_ngram = " ",
        chargram = FALSE,
        row.names = c(6L, 1L, 2L, 3L, 4L, 5L, 8L, 9L, 7L),
        class = c("mlvocab_vocab", "text2vec_vocabulary", "data.frame"))
    expect_equal(v, vt)

    v <- vocab(corpus, ngram = c(2, 3)) %>% .[order(.[["term_count"]]), ]
    vt <- structure(
        list(term = c("dog the", "The quick", "The quick brown", 
                      "lazy dog the", "dog the quick", "the quick brown", "the quick", 
                      "the lazy", "fox jumps", "lazy dog", "over the lazy", "jumps over", 
                      "over the", "quick brown fox", "brown fox jumps", "quick brown", 
                      "jumps over the", "brown fox", "fox jumps over", "the lazy dog"), 
             term_count = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), 
             doc_count = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,  2L)), 
        ngram = c(2L, 3L), 
        document_count = 2L, sep_ngram = " ", chargram = FALSE,
        row.names = c(10L, 11L, 15L, 17L, 18L, 7L, 12L, 1L, 2L, 3L, 4L, 5L, 6L, 8L, 9L, 13L, 14L, 16L, 19L, 20L), 
        class = c("mlvocab_vocab", "text2vec_vocabulary", "data.frame"))
    expect_equal(v, vt)
    
})

test_that("vocab adds new terms to the end", {
    v <- vocab(corpus)
    extras <- list(extras = c("apples", "oranges"))
    v2 <- vocab(c(corpus, extras))
    expect_equal(v2$term[-c(1:nrow(v))], extras$extras)
    expect_equal(v2, vocab_update(v, extras))
})

test_that("vocab_update works on text2vec_vocabulary", {
    v <- vocab(corpus)
    oldClass(v) <- c("text2vec_vocabulary", "data.frame")
    v3 <- vocab_update(v, list(tt = "tmp"))
    oldClass(v) <- c("mlvocab_vocab", "data.frame")
    v4 <- vocab_update(v, list(tt = "tmp"))
    expect_equal(v3, v4)
})
