context("index")

corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"), 
               b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
                     "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
vocab <- vocab(corpus)

test_that("text2seq works",  {

    corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the",
                         "lazy", "dog"), 
                   b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                         "dog", "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                         "dog"))
    vocab <- vocab(corpus)
    tcorpus <- c(corpus, list(c = c("dog", "eats", "dog")))

    ixs <- tiseq(tcorpus, vocab, keep_unknown = T)
    expect_equal(corpus$a, vocab$term[ixs$a])
    expect_equal(corpus$b, vocab$term[ixs$b])
    expect_equal(ixs$c[[2]], 0L)

    ixs <- tiseq(tcorpus, vocab, keep_unknown = F)
    expect_equal(ixs$c, c(9L, 9L))

    dogix <- which(vocab$term == "dog")

    expect_equal(timat(tcorpus, vocab, maxlen = 12, keep_unknown = F)[3, 1:3],
                 c(dogix, dogix, 0))
    expect_equal(timat(tcorpus, vocab, maxlen = 12, keep_unknown = T)[3, 1:3],
                 c(dogix, 0, dogix))
    expect_equal(timat(tcorpus, vocab, maxlen = 12, keep_unknown = T)[, 9],
                 c(a = dogix, b = dogix, c = 0))
    expect_equal(timat(tcorpus, vocab, maxlen = 12, pad_right = F, keep_unknown = T)[3, 9:12],
                 c(0, dogix, 0, dogix))

    expect_equal(timat(tcorpus, vocab, maxlen = 12, trunc_right = F, keep_unknown = T)[, 12],
                 c(a = 0, b = dogix, c = 0))
    expect_equal(timat(tcorpus, vocab, maxlen = 12, pad_right = F, trunc_right = F, keep_unknown = T)[, 12],
                 c(a = dogix, b = dogix, c = dogix))

})

test_that("tiseq preserves order", {
    expect_equal(tiseq(list(vocab$term), vocab)[[1]], 1:nrow(vocab))
})

test_that("text2seq with bucketing works",  {

    corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the",
                         "lazy", "dog"), 
                   b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                         "dog", "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                         "dog"))
    vocab <- vocab(corpus)
    tcorpus <- c(corpus, list(c = c("dog", "eats", "apples", "and", "oranges")))

    nterms <- length(unique(unlist(corpus)))
    ixs <- tiseq(tcorpus, vocab, nbuckets = 2)
    expect_equal(sort(unique(unlist(ixs))), 1:(nterms + 2))

    ixs <- tiseq(tcorpus, vocab, nbuckets = 100)
    expect_equal(length(unique(unlist(ixs))), nterms + 4)
})

test_that("murmur3hash works", {
    expect_equal(length(unique(murmur3hash(letters))), length(letters))
})


test_that("encodding doesn't matter", {

    txt <- c("”", "“", "–", "’", "…", "—", "‘", "•", "»", 
             "·", "�", "£", "«", "→", "®", "🙂", "←", "€", "™", 
             "©", "﻿", "­", "​", "−", "\u0093", "\u0094", "›", "\u0097", 
             "×", "§")

    v <- vocab(txt)

    txt1 <- txt
    Encoding(txt1) <- "UTF-8"
    v1 <- vocab(txt1)

    txt2 <- txt
    Encoding(txt[1:5]) <- "native"
    Encoding(txt[6:10]) <- "latin1"
    v2 <- vocab(txt2)

    mat <- timat(list(txt, txt1, txt2), v)
    expect_equal(mat[1, ], mat[2, ])
    expect_equal(mat[1, ], mat[3, ])

    mat <- timat(list(txt, txt1, txt2), v1)
    expect_equal(mat[1, ], mat[2, ])
    expect_equal(mat[1, ], mat[3, ])

    mat <- timat(list(txt, txt1, txt2), v2, nbuckets = 10)
    expect_equal(mat[1, ], mat[2, ])
    expect_equal(mat[1, ], mat[3, ])

})
