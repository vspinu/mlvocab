context("indices")

corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"), 
               b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
                     "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
scorpus <- sapply(corpus, paste, collapse = " ")
dcorpus <- data.frame(names = names(corpus), stringsAsFactors = F)
dcorpus$corpus <- corpus
dscorpus <- data.frame(names = names(scorpus), corpus = unname(scorpus), stringsAsFactors = F)

test_that("text2seq works",  {

  vocab <- vocab(corpus)
  tcorpus <- c(corpus, list(c = c("dog", "eats", "dog")))
  tdcorpus <- rbind(dcorpus, list(names = "c", corpus = list(c("dog", "eats", "dog"))))
  tdscorpus <- rbind(dscorpus, list(names = "c", corpus = "dog eats dog"))

  ixs <- tix_seq(tcorpus, vocab, keep_unknown = T)
  expect_equal(corpus$a, vocab$term[ixs$a])
  expect_equal(corpus$b, vocab$term[ixs$b])
  expect_equal(ixs$c[[2]], 0L)
  expect_equal(ixs, tix_seq(tdcorpus, vocab, keep_unknown = T))
  expect_equal(ixs, tix_seq(tdscorpus, vocab, keep_unknown = T))

  ixs <- tix_seq(tcorpus, vocab, keep_unknown = F)
  expect_equal(ixs$c, c(9L, 9L))

  dogix <- which(vocab$term == "dog")

  expect_equal(tix_mat(tcorpus, vocab, maxlen = 12, keep_unknown = F)[3, 1:3],
               c(dogix, dogix, 0))
  expect_equal(tix_mat(tcorpus, vocab, maxlen = 12, keep_unknown = T)[3, 1:3],
               c(dogix, 0, dogix))
  expect_equal(tix_mat(tcorpus, vocab, maxlen = 12, keep_unknown = T)[, 9],
               c(a = dogix, b = dogix, c = 0))
  expect_equal(tix_mat(tcorpus, vocab, maxlen = 12, pad_right = F, keep_unknown = T)[3, 9:12],
               c(0, dogix, 0, dogix))

  expect_equal(tix_mat(tcorpus, vocab, maxlen = 12, keep_unknown = F),
               tix_mat(tdcorpus, vocab, maxlen = 12, keep_unknown = F))
  expect_equal(tix_mat(tcorpus, vocab, maxlen = 12, keep_unknown = F),
               tix_mat(tdscorpus, vocab, maxlen = 12, keep_unknown = F))

  expect_equal(tix_mat(tcorpus, vocab, maxlen = 12, trunc_right = F, keep_unknown = T)[, 12],
               c(a = 0, b = dogix, c = 0))
  expect_equal(tix_mat(tcorpus, vocab, maxlen = 12, pad_right = F, trunc_right = F, keep_unknown = T)[, 12],
               c(a = dogix, b = dogix, c = dogix))

  expect_equal(tix_mat(tcorpus, vocab, maxlen = 12, trunc_right = F, keep_unknown = T),
               tix_mat(tdcorpus, vocab, maxlen = 12, trunc_right = F, keep_unknown = T))
  expect_equal(tix_mat(tcorpus, vocab, maxlen = 12, trunc_right = F, keep_unknown = T),
               tix_mat(tdscorpus, vocab, maxlen = 12, trunc_right = F, keep_unknown = T))
  
})

test_that("text2ixdf works", {
  
  vocab <- vocab(corpus)
  tcorpus <- c(corpus, list(c = c("dog", "eats", "dog")))
  tdcorpus <- rbind(dcorpus, list(names = "c", corpus = list(c("dog", "eats", "dog"))))
  tdscorpus <- rbind(dscorpus, list(names = "c", corpus = "dog eats dog"))

  df <- tix_df(tcorpus, vocab)
  expect_equal(df$ix, unname(do.call(c, tix_seq(tcorpus, vocab))))
  expect_equal(df$doc, gsub("[0-9]", "", names(do.call(c, tix_seq(tcorpus, vocab)))))
  df <- tix_df(tdcorpus, vocab)
  expect_equal(df$ix, unname(do.call(c, tix_seq(tdcorpus, vocab))))
  expect_equal(df$doc, gsub("[0-9]", "", names(do.call(c, tix_seq(tdcorpus, vocab)))))
  df <- tix_df(tdscorpus, vocab)
  expect_equal(df$ix, unname(do.call(c, tix_seq(tdscorpus, vocab))))
  expect_equal(df$doc, gsub("[0-9]", "", names(do.call(c, tix_seq(tdscorpus, vocab)))))  
})

test_that("text2ixdf works with no names", {

  vocab <- vocab(corpus)
  tcorpus <- unname(corpus)
  tscorpus <- unname(scorpus)
  id <- as.integer(as.factor(gsub("[0-9]", "", names(do.call(c, tix_seq(corpus, vocab))))))

  df <- tix_df(tcorpus, vocab)
  expect_equal(df$ix, unname(do.call(c, tix_seq(corpus, vocab))))
  expect_equal(df$doc, id)

  df <- tix_df(tscorpus, vocab)
  expect_equal(df$ix, unname(do.call(c, tix_seq(corpus, vocab))))
  expect_equal(df$doc, id)
})


test_that("text2ixdf works with various name types", {
  vocab <- vocab(corpus)
  tdcorpus <- rbind(dcorpus, list(names = "c", corpus = list(c("dog", "eats", "dog"))))
  tdscorpus <- rbind(dscorpus, list(names = "c", corpus = "dog eats dog"))
  id <- as.integer(as.factor(gsub("[0-9]", "", names(do.call(c, tix_seq(tdcorpus, vocab))))))

  ttdcorpus <- tdcorpus
  ttdcorpus$names <- as.integer(as.factor(tdcorpus$names))
  df <- tix_df(ttdcorpus, vocab)
  expect_identical(df$doc, id)
  
  ttdcorpus <- tdcorpus
  ttdcorpus$names <- as.numeric(as.factor(tdcorpus$names))
  df <- tix_df(ttdcorpus, vocab)
  expect_identical(df$doc, as.numeric(id))

  ttdcorpus <- tdcorpus
  ttdcorpus$names <- as.factor(tdcorpus$names)
  df <- tix_df(ttdcorpus, vocab)
  expect_identical(as.integer(df$doc), id)
  expect_identical(levels(df$doc), levels(ttdcorpus$names))
  expect_identical(class(df$doc), class(ttdcorpus$names))

  ttdcorpus <- tdcorpus
  ttdcorpus$names <- TRUE
  expect_error(tix_df(ttdcorpus, vocab))
})

test_that("text2ixseq works with no names", {
  vocab <- vocab(corpus)
  tcorpus <- unname(corpus)
  tscorpus <- unname(scorpus)
  expect_equal(unname(tix_mat(corpus, vocab, maxlen = 12, keep_unknown = F)),
               tix_mat(tcorpus, vocab, maxlen = 12, keep_unknown = F))
  expect_equal(unname(tix_mat(corpus, vocab, maxlen = 12, keep_unknown = F)),
               tix_mat(tscorpus, vocab, maxlen = 12, keep_unknown = F))
})

test_that("text2ixseq works with various name types", {
  vocab <- vocab(corpus)
  tdcorpus <- rbind(dcorpus, list(names = "c", corpus = list(c("dog", "eats", "dog"))))
  tdscorpus <- rbind(dscorpus, list(names = "c", corpus = "dog eats dog"))

  nms <- c("a", "b", "c")
  seq <- tix_seq(tdcorpus, vocab)
  expect_identical(names(seq), nms)

  ttdcorpus <- tdcorpus
  ttdcorpus$names <- as.integer(as.factor(tdcorpus$names))
  seq <- tix_seq(ttdcorpus, vocab)
  expect_identical(names(seq), as.character(1:3))
  
  ttdcorpus <- tdcorpus
  ttdcorpus$names <- as.numeric(as.factor(tdcorpus$names))
  seq <- tix_seq(ttdcorpus, vocab)
  expect_identical(names(seq), as.character(1:3))

  ttdcorpus <- tdcorpus
  ttdcorpus$names <- as.factor(tdcorpus$names)
  seq <- tix_seq(ttdcorpus, vocab)
})

test_that("tix_seq preserves order", {
  vocab <- vocab(corpus)
  expect_equal(tix_seq(list(vocab$term), vocab)[[1]],
               1:nrow(vocab))
})

test_that("text2seq with bucketing works",  {

    vocab <- vocab(corpus)
    tcorpus <- c(corpus, list(c = c("dog", "eats", "apples", "and", "oranges")))

    nterms <- length(unique(unlist(corpus)))
    ixs <- tix_seq(tcorpus, vocab, nbuckets = 2)
    expect_equal(sort(unique(unlist(ixs))), 1:(nterms + 2))

    ixs <- tix_seq(tcorpus, vocab, nbuckets = 100)
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

    mat <- tix_mat(list(txt, txt1, txt2), v)
    expect_equal(mat[1, ], mat[2, ])
    expect_equal(mat[1, ], mat[3, ])

    mat <- tix_mat(list(txt, txt1, txt2), v1)
    expect_equal(mat[1, ], mat[2, ])
    expect_equal(mat[1, ], mat[3, ])

    mat <- tix_mat(list(txt, txt1, txt2), v2, nbuckets = 10)
    expect_equal(mat[1, ], mat[2, ])
    expect_equal(mat[1, ], mat[3, ])

})
