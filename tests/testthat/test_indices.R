context("indices")
corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"), 
               b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
                     "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
scorpus <- sapply(corpus, paste, collapse = " ")
dcorpus <- data.frame(names = names(corpus), stringsAsFactors = F)
dcorpus$corpus <- corpus
dscorpus <- data.frame(names = names(scorpus), corpus = unname(scorpus), stringsAsFactors = F)
vocab <- vocab(corpus, regex = " ")

test_that("text2seq works",  {

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
  expect_equal(ixs$c, c(8L, 8L))

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
  
  tcorpus <- c(corpus, list(c = c("dog", "eats", "dog")))
  tdcorpus <- rbind(dcorpus, list(names = "c", corpus = list(c("dog", "eats", "dog"))))
  tdscorpus <- rbind(dscorpus, list(names = "c", corpus = "dog eats dog"))

  df <- tix_df(tcorpus, vocab)
  expect_equal(df$term, unname(do.call(c, tix_seq(tcorpus, vocab))))
  expect_equal(df$id, gsub("[0-9]", "", names(do.call(c, tix_seq(tcorpus, vocab)))))
  df <- tix_df(tdcorpus, vocab)
  expect_equal(df$corpus_term, unname(do.call(c, tix_seq(tdcorpus, vocab))))
  expect_equal(df$names, gsub("[0-9]", "", names(do.call(c, tix_seq(tdcorpus, vocab)))))
  df <- tix_df(tdscorpus, vocab)
  expect_equal(df$corpus_term, unname(do.call(c, tix_seq(tdscorpus, vocab))))
  expect_equal(df$names, gsub("[0-9]", "", names(do.call(c, tix_seq(tdscorpus, vocab)))))  
})

test_that("text2ixdf works with no names", {

  tcorpus <- unname(corpus)
  tscorpus <- unname(scorpus)
  id <- as.integer(as.factor(gsub("[0-9]", "", names(do.call(c, tix_seq(corpus, vocab))))))

  df <- tix_df(tcorpus, vocab)
  expect_equal(df$term, unname(do.call(c, tix_seq(corpus, vocab))))
  expect_equal(df$id, id)

  df <- tix_df(tscorpus, vocab)
  expect_equal(df$term, unname(do.call(c, tix_seq(corpus, vocab))))
  expect_equal(df$id, id)
})


test_that("text2ixdf works with various name types", {

  tdcorpus <- rbind(dcorpus, list(names = "c", corpus = list(c("dog", "eats", "dog"))))
  tdscorpus <- rbind(dscorpus, list(names = "c", corpus = "dog eats dog"))
  id <- as.integer(as.factor(gsub("[0-9]", "", names(do.call(c, tix_seq(tdcorpus, vocab))))))

  ttdcorpus <- tdcorpus
  ttdcorpus$names <- as.integer(as.factor(tdcorpus$names))
  df <- tix_df(ttdcorpus, vocab)
  expect_identical(df[[1]], id)
  
  ttdcorpus <- tdcorpus
  ttdcorpus$names <- as.numeric(as.factor(tdcorpus$names))
  df <- tix_df(ttdcorpus, vocab)
  expect_identical(df[[1]], as.numeric(id))

  ttdcorpus <- tdcorpus
  ttdcorpus$names <- as.factor(tdcorpus$names)
  df <- tix_df(ttdcorpus, vocab)
  expect_identical(as.integer(df[[1]]), id)
  expect_identical(levels(df[[1]]), levels(ttdcorpus$names))
  expect_identical(class(df[[1]]), class(ttdcorpus$names))

  ttdcorpus <- tdcorpus
  ttdcorpus$names <- TRUE
  expect_equal(tix_df(ttdcorpus, vocab)$names, rep.int(T, 29))
})

test_that("text2ixdf works multiple id columns", {

  dcorpus2 <- rbind(dcorpus, list(names = "c", corpus = list(c("dog", "eats", "dog"))))
  N <- nrow(dcorpus2)
  set.seed(1)
  ids <- data.frame(id0 = factor(1:N),
                    id1 = runif(N),
                    id2 = rep.int(T, N),
                    id3 = .POSIXct(rep.int(0, N)),
                    id4 = as.character(1:N),
                    id5 = 1:N)
  tdcorpus <- cbind(ids, dcorpus2)
  seq <- tix_seq(tdcorpus, vocab)

  df <- tix_df(tdcorpus, vocab)
  old_ids <- tdcorpus[, -ncol(tdcorpus)]
  new_ids <- df[, -ncol(df)]
  expect_equal(names(old_ids), names(new_ids))
  expect_equal(sapply(old_ids, class), sapply(new_ids, class))
  expect_equal(unname(split(df$corpus_term, df[[1]])), seq)
  
})

test_that("text2ixdf works with return_factor=T", {
  df <- tix_df(dcorpus, vocab)
  df$corpus_term <- structure(df$corpus_term, levels = vocab$term, class = "factor")
  expect_equal(tix_df(dcorpus, vocab, as_factor = T), df)

  tvocab <- prune_vocab(vocab(corpus), max_terms = 3, nbuckets = 3)
  df <- tix_df(dcorpus, tvocab)
  df$corpus_term <- structure(df$corpus_term, levels = tvocab$term, class = "factor")
  expect_equal(tix_df(dcorpus, tvocab, as_factor = T), df)
})

test_that("text2ixseq works with no names", {
  tcorpus <- unname(corpus)
  tscorpus <- unname(scorpus)
  expect_equal(unname(tix_mat(corpus, vocab, maxlen = 12, keep_unknown = F)),
               tix_mat(tcorpus, vocab, maxlen = 12, keep_unknown = F))
  expect_equal(unname(tix_mat(corpus, vocab, maxlen = 12, keep_unknown = F)),
               tix_mat(tscorpus, vocab, maxlen = 12, keep_unknown = F))
})

test_that("text2ixseq works with various name types", {

  tdcorpus <- rbind(dcorpus, list(names = "c", corpus = list(c("dog", "eats", "dog"))))
  tdscorpus <- rbind(dscorpus, list(names = "c", corpus = "dog eats dog"))

  nms <- c("a", "b", "c")
  seq <- tix_seq(tdcorpus, vocab)
  expect_identical(names(seq), nms)

  ttdcorpus <- tdcorpus
  ttdcorpus$names <- as.integer(as.factor(tdcorpus$names))
  seq <- tix_seq(ttdcorpus, vocab)
  ## no automatic promotion of integer column
  expect_null(names(seq))
  
  ttdcorpus <- tdcorpus
  ttdcorpus$names <- as.numeric(as.factor(tdcorpus$names))
  seq <- tix_seq(ttdcorpus, vocab)
  ## no automatic promotion of integer column
  expect_null(names(seq))

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
