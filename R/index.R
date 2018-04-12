

##' Convert text to integer indexes
##'
##' @param corpus text corpus
##' @param vocab data frame produced by [vocab()] or [vocab_update()]
##' @param keep_unknown logical. If `TRUE`, preserve unknowns in the output
##'   sequences.
##' @param nbuckets integer. How many buckets to hash unknowns into.
##' @return [tiseq()] returns a list of integer vectors, [timat()] returns an
##'   integer matrix, one row per sequence.
##' @rdname term_index
##' @export
tiseq <- function(corpus, vocab,
                   keep_unknown = nbuckets > 0,
                   nbuckets = attr(vocab, "nbuckets"),
                   reverse = FALSE) {
  structure(C_corpus2ixseq(corpus, vocab,  keep_unknown, nbuckets, reverse),
            names = names(corpus))
}

##' @param maxlen integer. Maximum length of each sequence.
##' @param pad_right logical. Should 0-padding of shorter than `maxlen`
##'   sequences happen on the right? Default `TRUE`.
##' @param trunc_right logical. Should truncation of longer than `maxlen`
##'   sequences happen on the right? Default `TRUE`.
##' @param reverse logical. Should each sequence be reversed in the final
##'   output? Reversion happens after `pad_right` and `trunc_right` have been
##'   applied to the original text sequence. Default `FALSE`.
##' @rdname term_index
##' @export
timat <- function(corpus, vocab, maxlen = 100, pad_right = TRUE, trunc_right = TRUE,
                   keep_unknown = nbuckets > 0,
                   nbuckets = attr(vocab, "nbuckets"),
                   reverse = FALSE) {
  out <- C_corpus2ixmat(corpus, vocab, maxlen, pad_right, trunc_right,
                        keep_unknown, nbuckets, reverse)
  if (!is.null(names(corpus)))
    rownames(out) <- names(corpus)
  out
}
