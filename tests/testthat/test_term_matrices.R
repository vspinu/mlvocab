context("term_matrices")
## library(mlvocab)
corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the",
                     "lazy", "dog"), 
               b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                     "dog", "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                     "dog"))
v <- vocab(corpus)
mat <- structure(c(1, 0, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 4, 1, 2, 1, 2),
                 .Dim = c(2L, 9L),
                 .Dimnames = list(c("a", "b"), c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog")))

test_that("dtm works", {
  dtm <- dtm(corpus, v)
  expect_s4_class(dtm, "dgTMatrix")
  expect_equal(as.matrix(dtm), mat)

  dtm <- dtm(corpus, v, output = "column")
  expect_s4_class(dtm, "dgCMatrix")
  expect_equal(as.matrix(dtm), mat)

  dtm <- dtm(corpus, v, output = "row")
  expect_s4_class(dtm, "dgRMatrix")
  expect_equal(as.matrix(dtm), mat)

  names(corpus) <- NULL
  rownames(mat) <- NULL

  dtm <- dtm(corpus, v, output = "triplet")
  expect_s4_class(dtm, "dgTMatrix")
  expect_equal(as.matrix(dtm), mat)

  dtm <- dtm(corpus, v, output = "column")
  expect_s4_class(dtm, "dgCMatrix")
  expect_equal(as.matrix(dtm), mat)

  dtm <- dtm(corpus, v, output = "row")
  expect_s4_class(dtm, "dgRMatrix")
  expect_equal(as.matrix(dtm), mat)
})

test_that("tdm works", {

  mat <- t(mat)
  tdm <- tdm(corpus, v)
  expect_s4_class(tdm, "dgTMatrix")
  expect_equal(as.matrix(tdm), mat)

  tdm <- tdm(corpus, v, output = "column")
  expect_s4_class(tdm, "dgCMatrix")
  expect_equal(as.matrix(tdm), mat)

  tdm <- tdm(corpus, v, output = "row")
  expect_s4_class(tdm, "dgRMatrix")
  expect_equal(as.matrix(tdm), mat)

  names(corpus) <- NULL
  colnames(mat) <- NULL

  tdm <- tdm(corpus, v, output = "triplet")
  expect_s4_class(tdm, "dgTMatrix")
  expect_equal(as.matrix(tdm), mat)

  tdm <- tdm(corpus, v, output = "column")
  expect_s4_class(tdm, "dgCMatrix")
  expect_equal(as.matrix(tdm), mat)

  tdm <- tdm(corpus, v, output = "row")
  expect_s4_class(tdm, "dgRMatrix")
  expect_equal(as.matrix(tdm), mat)
})
