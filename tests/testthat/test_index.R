library(mlstring)
library(testthat)

corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the",
                     "lazy", "dog"), 
               b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                     "dog", "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                     "dog"))
vocab <- vocab(corpus)

test_that("corpus2seq works",  {

    corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the",
                         "lazy", "dog"), 
                   b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                         "dog", "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                         "dog"))
    vocab <- vocab(corpus)
    tcorpus <- c(corpus, list(c = c("dog", "eats", "dog")))

    ixs <- corpus2ixseq(tcorpus, vocab, keep_unknown = T)
    expect_equal(corpus$a, vocab$term[ixs$a])
    expect_equal(corpus$b, vocab$term[ixs$b])
    expect_equal(ixs$c[[2]], 0L)

    ixs <- corpus2ixseq(tcorpus, vocab, keep_unknown = F)
    expect_equal(ixs$c, c(2L, 2L))

    dogix <- which(vocab$term == "dog")

    expect_equal(corpus2ixmat(tcorpus, vocab, maxlen = 12, keep_unknown = F)[3, 1:3],
                 c(dogix, dogix, 0))
    expect_equal(corpus2ixmat(tcorpus, vocab, maxlen = 12, keep_unknown = T)[3, 1:3],
                 c(dogix, 0, dogix))
    expect_equal(corpus2ixmat(tcorpus, vocab, maxlen = 12, keep_unknown = T)[, 9],
                 c(a = dogix, b = dogix, c = 0))
    expect_equal(corpus2ixmat(tcorpus, vocab, maxlen = 12, pad_right = F, keep_unknown = T)[3, 9:12],
                 c(0, dogix, 0, dogix))

    expect_equal(corpus2ixmat(tcorpus, vocab, maxlen = 12, trunc_right = F, keep_unknown = T)[, 12],
                 c(a = 0, b = dogix, c = 0))
    expect_equal(corpus2ixmat(tcorpus, vocab, maxlen = 12, pad_right = F, trunc_right = F, keep_unknown = T)[, 12],
                 c(a = dogix, b = dogix, c = dogix))

})

test_that("doc2ixseq preserves order", {
    expect_equal(doc2ixseq(vocab$term, vocab), 1:nrow(vocab))
})

test_that("corpus2seq with bucketing works",  {

    corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the",
                         "lazy", "dog"), 
                   b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                         "dog", "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                         "dog"))
    vocab <- vocab(corpus)
    tcorpus <- c(corpus, list(c = c("dog", "eats", "apples", "and", "oranges")))

    nterms <- length(unique(unlist(corpus)))
    ixs <- corpus2ixseq(tcorpus, vocab, unknown_buckets = 2)
    expect_equal(sort(unique(unlist(ixs))), 1:(nterms + 2))

    ixs <- corpus2ixseq(tcorpus, vocab, unknown_buckets = 100)
    expect_equal(length(unique(unlist(ixs))), nterms + 4)
})

test_that("murmur3hash works", {
    expect_equal(length(unique(murmur3hash(letters))), length(letters))
})
