context("tfidf")

corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"),
               b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
                     "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
nms <- unique(unlist(corpus))
nms2 <- mlvocab:::C_wordgram(nms, 1, 2, " ")

test_that("tfidf works with tdm and dtm matrices", {

  v <- vocab(corpus, c(1, 2), " ")
  dtm <- dtm(corpus, v, output = "col")
  tdm <- tdm(corpus, v)

  expect_equal(as.matrix(dtm), as.matrix(dtm(corpus, v, output = "row")))
  expect_equal(as.matrix(dtm), as.matrix(dtm(corpus, v, output = "tri")))

  ## tt <- text2vec::TfIdf$new()
  ## dput(out <- tt$fit_transform(dtm))
  out <- new("dgCMatrix",
             i = c(0L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L,
                   0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L,
                   0L, 1L, 0L, 1L, 0L, 1L, 1L, 1L),
             p = c(0L, 1L, 2L, 4L, 6L, 8L, 10L, 12L, 14L, 16L, 18L, 20L, 22L, 24L, 26L, 28L, 30L, 32L, 33L, 34L),
             Dim = c(2L, 19L),
             Dimnames = list(c("a", "b"),
                             c("The",
                               "The quick", "quick", "quick brown", "brown", "brown fox", "fox",
                               "fox jumps", "jumps", "jumps over", "over", "over the", "the",
                               "the lazy", "lazy", "lazy dog", "dog", "the quick", "dog the"
                               )),
             x = c(0, 0, -0.023850888712245, -0.023169434749038, -0.023850888712245,
                   -0.023169434749038, -0.023850888712245, -0.023169434749038, -0.023850888712245,
                   -0.023169434749038, -0.023850888712245, -0.023169434749038, -0.023850888712245,
                   -0.023169434749038, -0.023850888712245, -0.023169434749038, -0.023850888712245,
                   -0.023169434749038, -0.023850888712245, -0.023169434749038, -0.023850888712245,
                   -0.023169434749038, -0.023850888712245, -0.0463388694980759,
                   -0.023850888712245, -0.023169434749038, -0.023850888712245, -0.023169434749038,
                   -0.023850888712245, -0.023169434749038, -0.023850888712245, -0.023169434749038,
                   0, 0),
             factors = list())

  expect_equal(out[, nms2], tfidf(dtm, v)[, nms2])
  tout <- t(out)
  expect_equal(tout[nms2, ], tfidf(tdm, v)[nms2, ])

  out <- new("dgCMatrix",
             i = c(0L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L,
                   0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L,
                   0L, 1L, 0L, 1L, 0L, 1L, 1L, 1L),
             p = c(0L, 1L, 2L, 4L, 6L, 8L,
                   10L, 12L, 14L, 16L, 18L, 20L, 22L, 24L, 26L, 28L, 30L, 32L, 33L,
                   34L),
             Dim = c(2L, 19L),
             Dimnames = list(c("a", "b"),
                             c("The", "The quick", "quick", "quick brown", "brown", "brown fox", "fox", "fox jumps", "jumps", "jumps over", "over", "over the", "the", "the lazy", "lazy", "lazy dog", "dog", "the quick", "dog the")),
             x = c(0, 0, -0.023850888712245, -0.023850888712245, -0.023850888712245, -0.023850888712245, -0.023850888712245, -0.023850888712245, -0.023850888712245,
                   -0.023850888712245, -0.023850888712245, -0.023850888712245, -0.023850888712245,
                   -0.023850888712245, -0.023850888712245, -0.023850888712245, -0.023850888712245,
                   -0.023850888712245, -0.023850888712245, -0.023850888712245, -0.023850888712245,
                   -0.023850888712245, -0.023850888712245, -0.0336150583335027,
                   -0.023850888712245, -0.023850888712245, -0.023850888712245, -0.023850888712245,
                   -0.023850888712245, -0.023850888712245, -0.023850888712245, -0.023850888712245,
                   0, 0),
             factors = list())

  expect_equal(out[, nms2], tfidf(dtm, v, sublinear_tf = T)[, nms2])
  tout <- t(out)
  expect_equal(tout[nms2, ], tfidf(tdm, v, sublinear_tf = T)[nms2, ])

  ## tt <- text2vec::TfIdf$new(sublinear_tf = T, smooth_idf = F)
  ## dput(out <- tt$fit_transform(dtm))
  out <- new("dgCMatrix",
             i = c(0L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L,
                   0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L,
                   0L, 1L, 0L, 1L, 0L, 1L, 1L, 1L),
             p = c(0L, 1L, 2L, 4L, 6L, 8L, 10L, 12L, 14L, 16L, 18L, 20L, 22L, 24L, 26L, 28L, 30L, 32L, 33L, 34L),
             Dim = c(2L, 19L),
             Dimnames = list(c("a", "b"),
                             c("The", "The quick", "quick", "quick brown", "brown", "brown fox", "fox", "fox jumps", "jumps", "jumps over", "over", "over the", "the", "the lazy", "lazy", "lazy dog", "dog", "the quick", "dog the")),
             x = c(0.0407733635623497, 0.0407733635623497, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0407733635623497, 0.0240814053441387),
             factors = list())

  expect_equal(out[, nms2], tfidf(dtm, v, sublinear_tf = T, extra_df_count = 0)[, nms2])
  tout <- t(out)
  expect_equal(tout[nms2, ], tfidf(tdm, v, sublinear_tf = T, extra_df_count = 0)[nms2, ])

})


test_that("tfidf works when dtm is constructed with explicit ngram", {

  library(Matrix)
  v <- vocab(corpus, c(1, 2), " ")

  dtm <- dtm(corpus, v, c(1, 1), output = "col")
  tdm <- tdm(corpus, v, c(1, 1))

  expect_equal(as.matrix(dtm), as.matrix(dtm(corpus, v, c(1, 1), output = "row")))
  expect_equal(as.matrix(dtm), as.matrix(dtm(corpus, v, c(1, 1), output = "tri")))

  ## tt <- text2vec::TfIdf$new()
  ## dput(out <- tt$fit_transform(dtm))
  out <- new("dgCMatrix",
             i = c(0L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L,
                   1L, 0L, 1L, 0L, 1L, 0L, 1L),
             p = c(0L, 1L, 1L, 3L, 3L, 5L, 5L, 7L, 7L, 9L, 9L, 11L, 11L, 13L, 13L, 15L, 15L, 17L, 17L, 17L),
             Dim = c(2L, 19L),
             Dimnames = list(c("a", "b"),
                             c("The", "The quick",
                               "quick", "quick brown", "brown", "brown fox", "fox", "fox jumps",
                               "jumps", "jumps over", "over", "over the", "the", "the lazy",
                               "lazy", "lazy dog", "dog", "the quick", "dog the")),
             x = c(0, -0.0450516786786849, -0.0450516786786849, -0.0450516786786849,
                   -0.0450516786786849, -0.0450516786786849, -0.0450516786786849,
                   -0.0450516786786849, -0.0450516786786849, -0.0450516786786849,
                   -0.0450516786786849, -0.0450516786786849, -0.0901033573573699,
                   -0.0450516786786849, -0.0450516786786849, -0.0450516786786849,
                   -0.0450516786786849),
             factors = list())

  expect_equal(out[, nms2], tfidf(dtm, v)[, nms2])
  tout <- t(out)
  expect_equal(tout[nms2, ], tfidf(tdm, v)[nms2, ])

})

test_that("tfidf works when names don't match", {

  v <- vocab(corpus, c(1, 2), " ")
  dtm <- dtm(corpus, v, out = "col")
  tdm <- tdm(corpus, v)
  v <- rbind(v, data.frame(term = "blabla", term_count = 0, doc_count = 0))

  expect_equal(as.matrix(dtm(corpus, v, output = "col")), as.matrix(dtm(corpus, v, output = "row")))
  expect_equal(as.matrix(dtm(corpus, v, output = "col")), as.matrix(dtm(corpus, v, output = "tri")))

  ## tt <- text2vec::TfIdf$new()
  ## dput(out <- tt$fit_transform(dtm))
  out <- new("dgCMatrix",
             i = c(0L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L,
                   0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L,
                   0L, 1L, 0L, 1L, 0L, 1L, 1L, 1L),
             p = c(0L, 1L, 2L, 4L, 6L, 8L, 10L, 12L, 14L, 16L, 18L, 20L, 22L, 24L, 26L, 28L, 30L, 32L, 33L, 34L),
             Dim = c(2L, 19L),
             Dimnames = list(c("a", "b"),
                             c("The",
                               "The quick", "quick", "quick brown", "brown", "brown fox", "fox",
                               "fox jumps", "jumps", "jumps over", "over", "over the", "the",
                               "the lazy", "lazy", "lazy dog", "dog", "the quick", "dog the"
                               )),
             x = c(0, 0, -0.023850888712245, -0.023169434749038, -0.023850888712245,
                   -0.023169434749038, -0.023850888712245, -0.023169434749038, -0.023850888712245,
                   -0.023169434749038, -0.023850888712245, -0.023169434749038, -0.023850888712245,
                   -0.023169434749038, -0.023850888712245, -0.023169434749038, -0.023850888712245,
                   -0.023169434749038, -0.023850888712245, -0.023169434749038, -0.023850888712245,
                   -0.023169434749038, -0.023850888712245, -0.0463388694980759,
                   -0.023850888712245, -0.023169434749038, -0.023850888712245, -0.023169434749038,
                   -0.023850888712245, -0.023169434749038, -0.023850888712245, -0.023169434749038,
                   0, 0),
             factors = list())


  expect_equal(out[, nms2], tfidf(dtm, v)[, nms2])
  tout <- t(out)
  expect_equal(tout[nms2, ], tfidf(tdm, v)[nms2, ])

})



test_that("tcm bootstrap", {

  ## skip_on_cran()
  corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"),
                 b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
                       "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"),
                 c = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
                       "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"),
                 d = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
                       "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"),
                 e = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
                       "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
  v <- vocab(corpus, c(1, 2), " ")
  dtm <- dtm(corpus, v, output = "col")
  for(i in 1:1000) {
    expect_equal(as.matrix(dtm), as.matrix(dtm(corpus, v, output = "row")))
  }

})
