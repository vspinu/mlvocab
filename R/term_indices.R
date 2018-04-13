
##' Convert text to integer indices
##'
##' @param corpus text corpus
##' @param vocab data frame produced by [vocab()] or [vocab_update()]
##' @param keep_unknown logical. If `TRUE`, preserve unknowns in the output
##'   sequences.
##' @param nbuckets integer. How many buckets to hash unknowns into.
##' @return [tiseq()] returns a list of integer vectors, [timat()] returns an
##'   integer matrix, one row per sequence.
##' @name term_indices
##' @examples
##' corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"), 
##'                b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
##'                      "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
##' v <- vocab(corpus["b"]) # "The" is unknown
##' v
##' tiseq(corpus, v)
##' tiseq(corpus, v, keep_unknown = TRUE)
##' tiseq(corpus, v, nbuckets = 1)
##' tiseq(corpus, v, nbuckets = 3)
##'
##' timat(corpus, v, maxlen = 12)
##' timat(corpus, v, maxlen = 12, keep_unknown = TRUE)
##' timat(corpus, v, maxlen = 12, nbuckets = 1)
##' timat(corpus, v, maxlen = 12, nbuckets = 1, reverse = TRUE)
##' timat(corpus, v, maxlen = 12, pad_right = FALSE, nbuckets = 1)
##' timat(corpus, v, maxlen = 12, trunc_right = FALSE, nbuckets = 1)
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
##' @name term_indices
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
