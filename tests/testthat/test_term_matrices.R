context("term_matrices")
corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"),
               b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                     "dog", "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                     "dog"))
v <- vocab(corpus)
vocab <- vocab(corpus)
mat <- structure(c(1, 4, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 0),
                 .Dim = c(2L, 9L),
                 .Dimnames = list(c("a", "b"),
                                  c("the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog", "The")))
library(Matrix)


test_that("Basic tcm works", {

  tcm0 <- new("dsTMatrix",
              i = c(0L, 6L, 0L, 1L, 2L, 0L, 0L, 3L, 1L, 4L),
              j = c(5L, 7L, 7L, 8L, 3L, 1L, 6L, 4L, 2L, 5L),
              Dim = c(9L, 9L),
              Dimnames = list(c("the", "quick", "brown", "fox", "jumps",
                                "over", "lazy", "dog", "The"),
                              c("the", "quick", "brown", "fox",
                                "jumps", "over", "lazy", "dog", "The")),
              x = c(1.5, 1.5, 0.5, 0.5, 1.5, 1, 1.5, 1.5, 1.5, 1.5),
              uplo = "U",
              factors = list())

  tcm <- tcm(corpus, v, 2)
  ## converting to matrix as order might not be the same depending on internal sparse_map hash
  expect_equal(as.matrix(tcm), as.matrix(tcm0))

  tcm <- tcm(corpus, v, 3)
  expect_equal(tcm["The", "brown"], 1/3)
  expect_equal(tcm["the", "brown"], 2/3)

  ltcm0 <- new("dgTMatrix", i = c(1L, 6L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L),
               j = c(6L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L),
               Dim = c(9L, 9L),
               Dimnames = list(c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"),
                               c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog")),
               x = c(1, 0.5, 0.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
               factors = list())

  nms <- colnames(ltcm0)
  ltcm <- tcm(corpus, v, 2, context = "left")
  expect_equal(as.matrix(ltcm)[nms, nms], as.matrix(ltcm0))
  ltcm <- tcm(corpus, v, 3, context = "left")
  expect_equal(ltcm["fox", "brown"], 1.5)
  expect_equal(ltcm["dog", "the"], 1)

  rtcm0 <- new("dgTMatrix", i = c(8L, 6L, 7L, 0L, 1L, 2L, 3L, 4L, 5L, 6L),
               j = c(6L, 1L, 8L, 1L, 2L, 3L, 4L, 5L, 6L, 7L),
               Dim = c(9L, 9L),
               Dimnames = list(c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"),
                               c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog")),
               x = c(0.5, 1, 1.5, 0.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
               factors = list())
  nms <- colnames(rtcm0)
  rtcm <- tcm(corpus, v, 2, context = "right")
  expect_equal(as.matrix(rtcm)[nms, nms], as.matrix(rtcm0))
  rtcm <- tcm(corpus, v, 3, context = "right")
  expect_equal(rtcm["The", "brown"], 1/3)
  expect_equal(rtcm["the", "brown"], 2/3)

  m1 <- as(tcm(corpus, v, 3, context = "right"), "matrix")
  m2 <- as(tcm(corpus, v, 3, context = "right"), "matrix")
  expect_equal(m1, m2)

  rtcm <- as.matrix(tcm(corpus, v, 2, context = "right", output = "column"))
  ltcm <- as.matrix(tcm(corpus, v, 2, context = "left", output = "column"))
  expect_equal(rtcm, t(ltcm))

  rtcm <- as.matrix(tcm(corpus, v, 2, context = "right", output = "row"))
  ltcm <- as.matrix(tcm(corpus, v, 2, context = "left", output = "row"))
  expect_equal(ltcm, t(rtcm))
})

test_that("tcm works with ngrams", {
  v <- vocab(corpus, ngram = c(1, 3))
  tcm <- tcm(corpus[1], v, context = "right")
  w <- mlvocab:::ngram_weights(ngram_max = 3)

  ## compute by hand with outer distance
  tt <- structure(1:15, names = mlvocab:::C_wordgram(corpus[[1]], 1, 3, "_")[1:15])
  dist <- outer(tt, tt, function(x, y) abs(x - y) + 1)
  nms <- rownames(dist)
  utri <- upper.tri(dist)
  tcm0 <- as.matrix(tcm[nms, nms])

  expect_equal(tcm0[utri], unname(w[dist[utri]]))
  expect_true(all(tcm0[!utri] == 0))

  tcms <- as.matrix(tcm(corpus[1], v, context = "s"))
  tcms2 <- as.matrix(tcm(corpus[1], v, context = "r") + tcm(corpus[1], v, context = "l"))
  expect_equal(unname(tcms), tcms2)
})

test_that("tcm works with custom weights", {
  v <- vocab(corpus)
  nms <- unique(unlist(corpus))
  tcm <- tcm(corpus, v, window_weights = 1, context = "r")
  expect_equal(unique(tcm["The", ]), c(0, 1))
  expect_equal(unname(tcm["quick", nms]), c(0, 0, 3, 3, 3, 3, 0, 0, 0))
  expect_equal(unname(tcm["the", nms]), c(0, 3, 2, 2, 2, 0, 1, 3, 3))
})

test_that("tcm outputs same matrix irrespetive of output type", {

  expect_equal(as.matrix(tcm(corpus, v, 2, context = "right", output = "row")),
               as.matrix(tcm(corpus, v, 2, context = "right", output = "column")))
  expect_equal(as.matrix(tcm(corpus, v, 2, context = "right", output = "row")),
               as.matrix(tcm(corpus, v, 2, context = "right", output = "triplet")))

  expect_equal(as.matrix(tcm(corpus, v, 2, context = "left", output = "row")),
               as.matrix(tcm(corpus, v, 2, context = "left", output = "column")))
  expect_equal(as.matrix(tcm(corpus, v, 2, context = "left", output = "row")),
               as.matrix(tcm(corpus, v, 2, context = "left", output = "triplet")))

  expect_equal(as.matrix(tcm(corpus, v, 2, context = "s", output = "row")),
               as.matrix(tcm(corpus, v, 2, context = "s", output = "column")))
  expect_equal(as.matrix(tcm(corpus, v, 2, context = "s", output = "row")),
               as.matrix(tcm(corpus, v, 2, context = "s", output = "triplet")))

  expect_equal(as.matrix(tcm(corpus, v, 5, context = "right", output = "row")),
               as.matrix(tcm(corpus, v, 5, context = "right", output = "column")))
  expect_equal(as.matrix(tcm(corpus, v, 5, context = "right", output = "row")),
               as.matrix(tcm(corpus, v, 5, context = "right", output = "triplet")))

  expect_equal(as.matrix(tcm(corpus, v, 5, context = "left", output = "row")),
               as.matrix(tcm(corpus, v, 5, context = "left", output = "column")))
  expect_equal(as.matrix(tcm(corpus, v, 5, context = "left", output = "row")),
               as.matrix(tcm(corpus, v, 5, context = "left", output = "triplet")))

  expect_equal(as.matrix(tcm(corpus, v, 5, context = "s", output = "row")),
               as.matrix(tcm(corpus, v, 5, context = "s", output = "column")))
  expect_equal(as.matrix(tcm(corpus, v, 5, context = "s", output = "row")),
               as.matrix(tcm(corpus, v, 5, context = "s", output = "triplet")))

})

test_that("dtm output type is correct", {

  dtm <- dtm(corpus, vocab, output = "tri")
  expect_s4_class(dtm, "dgTMatrix")
  expect_equal(as.matrix(dtm), mat)

  dtm <- dtm(corpus, v, output = "column")
  expect_s4_class(dtm, "dgCMatrix")
  expect_equal(as.matrix(dtm), mat)

  dtm <- dtm(corpus, v)
  expect_s4_class(dtm, "dgRMatrix")
  expect_equal(as.matrix(dtm), mat)

  dtm <- dtm(corpus, v, output = "row")
  expect_s4_class(dtm, "dgRMatrix")
  expect_equal(as.matrix(dtm), mat)

})

test_that("dtm works with NULL names", {

  names(corpus) <- NULL
  rownames(mat) <- NULL

  dtm <- dtm(corpus, v, output = "triplet")
  expect_s4_class(dtm, "dgTMatrix")
  expect_equal(dim(dtm), dim(mat))
  expect_equal(as.matrix(dtm), mat)

  dtm <- dtm(corpus, v, output = "column")
  expect_s4_class(dtm, "dgCMatrix")
  expect_equal(dim(dtm), dim(mat))
  expect_equal(as.matrix(dtm), mat)

  dtm <- dtm(corpus, v, output = "row")
  expect_s4_class(dtm, "dgRMatrix")
  expect_equal(dim(dtm), dim(mat))
  expect_equal(as.matrix(dtm), mat)

})


test_that("tdm works", {

  mat <- t(mat)
  tdm <- tdm(corpus, v, output = "tri")
  expect_s4_class(tdm, "dgTMatrix")
  expect_equal(as.matrix(tdm), mat)

  tdm <- tdm(corpus, v)
  expect_s4_class(tdm, "dgCMatrix")
  expect_equal(as.matrix(tdm), mat)

  tdm <- tdm(corpus, v, output = "column")
  expect_s4_class(tdm, "dgCMatrix")
  expect_equal(as.matrix(tdm), mat)

  tdm <- tdm(corpus, v, output = "row")
  expect_s4_class(tdm, "dgRMatrix")
  expect_equal(as.matrix(tdm), mat)

})

test_that("tdm works with NULL names", {

  names(corpus) <- NULL
  mat <- t(mat)
  colnames(mat) <- NULL

  tdm <- tdm(corpus, v, output = "triplet")
  expect_s4_class(tdm, "dgTMatrix")
  expect_equal(dim(tdm), dim(mat))
  expect_equal(as.matrix(tdm), mat)

  tdm <- tdm(corpus, v, output = "column")
  expect_s4_class(tdm, "dgCMatrix")
  expect_equal(dim(tdm), dim(mat))
  expect_equal(as.matrix(tdm), mat)

  tdm <- tdm(corpus, v, output = "row")
  expect_s4_class(tdm, "dgRMatrix")
  expect_equal(dim(tdm), dim(mat))
  expect_equal(as.matrix(tdm), mat)

})

test_that("tdm, dtm and tcm work with vocab=NULL", {
  expect_equal(tdm(corpus, v, output = "triplet"), tdm(corpus, output = "triplet"))
  expect_equal(tdm(corpus, v, output = "column"), tdm(corpus, output = "column"))
  expect_equal(dtm(corpus, v, output = "column"), dtm(corpus, output = "column"))
  expect_equal(tcm(corpus, v, output = "column"), tcm(corpus, output = "column"))
})


test_that("tdm/dtm work with empty docs", {
  expect_equal(as.matrix(dtm(c(corpus, list(c = character())), v)),
               rbind(mat, c = 0))
  expect_equal(as.matrix(dtm(c(list(c = character()), corpus), v)),
               rbind(c = 0, mat))
  expect_equal(as.matrix(tdm(c(corpus, list(c = character())), v)),
               cbind(t(mat), c = 0))
  expect_equal(as.matrix(tdm(c(list(c = character()), corpus), v)),
               cbind(c = 0, t(mat)))
})
