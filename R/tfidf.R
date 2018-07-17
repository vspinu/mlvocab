
##' Tfidf re-weighting of `dtm` and `tdm` matrices
##'
##' @param mat output of [dtm()] or [tdm()] function
##' @param vocab output of [vocab()] or [update_vocab()]
##' @param norm normalization to apply for each document. Either "l1", "l2" or
##'   "none"
##' @param sublinear_tf when `TRUE` use `1 + log(tf)` instead of the raw `tf`
##' @param extra_df_count add this number to the document count; as if all terms
##'   in the vocabulary have been seen at least in this many documents.
##' @examples
##' corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"),
##'                b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
##'                      "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
##' v <- vocab(corpus, c(1, 2), " ")
##' dtm <- dtm(corpus, v)
##' tfidf(dtm, v)
##' tdm <- tdm(corpus, v)
##' tfidf(tdm, v)
##' @export
tfidf <- function(mat, vocab, norm = c("l1", "l2", "none"), sublinear_tf = FALSE, extra_df_count = 1) {
  if (is.null(is_dtm <- attr(mat, "mlvocab_dtm", TRUE)))
    stop("Not a mlvocab term matrix")
  names <- if (is_dtm) colnames(mat) else rownames(mat)
  ## fixme: implement an efficient check for this
  if (identical(names, vocab$term)) {
    doc_count <- vocab$doc_count
  } else {
    ixs <- match(names, vocab$term)
    if (any(is.na(ixs)))
      stop("Some `mat` terms are not in the vocabulary")
    doc_count <- vocab$doc_count[ixs]
  }
  ndocs <- attr(vocab, "document_count", FALSE)
  idf <- log(ndocs / (doc_count + extra_df_count))
  if (sublinear_tf)
    mat@x <- 1 + log(mat@x)
  norm <- match.arg(norm)
  normalize(mat, norm, idf, is_dtm)
}

normalize <- function(mat, norm, idf, byrow) {

  stopifnot(inherits(mat, "sparseMatrix"))
  if (norm == "none")
    return(mat)

  tfnorm <-
    switch(norm,
           l1 = 1 / if (byrow) rowSums(abs(mat)) else colSums(abs(mat)),
           l2 = 1 / if (byrow) sqrt(rowSums(mat^2)) else sqrt(colSums(mat^2)),
           stop("Invalid norm ", norm))
  tfnorm[is.infinite(tfnorm)] <- 0

  reverse <- FALSE
  if (inherits(mat, "dgRMatrix")) {
    ## Matrix method %*% doesn't work on dgRMatrix. Do a quick conversion trick
    ## without copying i.
    matc <- new("dgCMatrix", i = mat@j, p = mat@p,
                Dim = rev(mat@Dim), Dimnames = rev(mat@Dimnames),
                x = mat@x,
                factors = list())
    byrow <- !byrow
    out <-
      if (byrow) (Diagonal(x = tfnorm) %*% matc) %*% Diagonal(x = idf)
      else Diagonal(x = idf) %*% (matc %*% Diagonal(x = tfnorm))
    return(new("dgRMatrix", j = mat@j, p = mat@p,
               Dim = mat@Dim, Dimnames = mat@Dimnames,
               x = out@x,
               factors = list()))
  } else {
    if (byrow) (Diagonal(x = tfnorm) %*% mat) %*% Diagonal(x = idf)
    else Diagonal(x = idf) %*% (mat %*% Diagonal(x = tfnorm))
  }
}
