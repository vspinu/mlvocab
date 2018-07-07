
##' Term Indices: Convert text to integer indices
##'
##' @param corpus text corpus; see `[vocab()]`.
##' @param vocab data frame produced by [vocab()] or [vocab_update()]
##' @param keep_unknown logical. If `TRUE`, preserve unknowns in the output
##'   sequences.
##' @param nbuckets integer. How many buckets to hash unknowns into.
##' @return [tix_seq()] returns a list of integer vectors, [tix_df()] produces a
##'   flat index data.frame with two columns, [tix_mat()] returns an integer
##'   matrix, one row per sequence.
##' @name term_indices
##' @examples
##' corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"), 
##'                b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
##'                      "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
##' v <- vocab(corpus["b"]) # "The" is unknown
##' v
##' tix_seq(corpus, v)
##' tix_seq(corpus, v, keep_unknown = TRUE)
##' tix_seq(corpus, v, nbuckets = 1)
##' tix_seq(corpus, v, nbuckets = 3)
##'
##' tix_mat(corpus, v, maxlen = 12)
##' tix_mat(corpus, v, maxlen = 12, keep_unknown = TRUE)
##' tix_mat(corpus, v, maxlen = 12, nbuckets = 1)
##' tix_mat(corpus, v, maxlen = 12, nbuckets = 1, reverse = TRUE)
##' tix_mat(corpus, v, maxlen = 12, pad_right = FALSE, nbuckets = 1)
##' tix_mat(corpus, v, maxlen = 12, trunc_right = FALSE, nbuckets = 1)
##' @export
tix_seq <- function(corpus, vocab,
                   keep_unknown = nbuckets > 0,
                   nbuckets = attr(vocab, "nbuckets"),
                  reverse = FALSE) {
  C_corpus2ixseq(corpus, vocab, keep_unknown, nbuckets, reverse)
}

##' @name term_indices
##' @export
tix_df <- function(corpus, vocab,
                 keep_unknown = nbuckets > 0,
                 nbuckets = attr(vocab, "nbuckets"),
                 reverse = FALSE) {
  C_corpus2ixdf(corpus, vocab, keep_unknown, nbuckets, reverse)
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
tix_mat <- function(corpus, vocab, maxlen = 100, pad_right = TRUE, trunc_right = TRUE,
                   keep_unknown = nbuckets > 0,
                   nbuckets = attr(vocab, "nbuckets"),
                   reverse = FALSE) {
  C_corpus2ixmat(corpus, vocab, maxlen, pad_right, trunc_right,
                        keep_unknown, nbuckets, reverse)
}

