##' Subset embedding matrix using vocab terms
##'
##' [prune_embeddings()] subsets a (commonly large) pre-trained word-vector matrix
##' into a smaller, one vector per term, embedding matrix.
##'
##' [prune_embeddings()] is commonly used in conjunction with sequence generators
##' ([tix_mat()], [tix_seq()] and [tix_df()]). When a term in a corpus is not
##' present in a vocabulary (unknown), it is hashed into one of the `nbuckets`
##' buckets. Embeddings which are hashed into same bucket are averaged to
##' produce the embedding for that bucket. Maximum number of embeddings to
##' average per bucket is controled with `max_in_bucket` parameter.
##'
##' Similarly, when a term from the vocabulary is not present in the embedding
##' matrix (aka missing) `max_in_bucket` embeddings are averaged to produce the
##' missing embedding. Different buckets are used for "missing" and "unknown"
##' embeddings because `nbuckets` can be 0.
##'
##' @param vocab `data.frame` obtained from a call to [vocab()].
##' @param embeddings embeddings matrix. The terms dimension must be named. If
##'   both [colnames()] and [rownames()] are non-null, dimension with more
##'   elements is considered term-dimension.
##' @param nbuckets How many unknown buckets to create for unknown terms (terms
##'   in corpus not present in the vocabulary).
##' @param max_in_bucket At most this many embedding vectors will be averaged
##'   into each unknown or missing bucket (see details). Lower number results in
##'   faster processing. For large `nbuckets` this number might not be reached
##'   due to the finiteness of the `embeddings` vocabulary, or even result in
##'   `0` embeddings being hashed into a bucket producing `[0 0 ...]` embeddings
##'   for some buckets.
##'
##' @examples
##'
##' corpus <-
##'    list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"),
##'         b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
##'               "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
##'
##' v <- vocab(corpus)
##' v2 <- prune_vocab(v, max_terms = 7, nbuckets = 2)
##' enames <- c("the", "quick", "brown", "fox", "jumps")
##' emat <- matrix(rnorm(50), nrow = 5, dimnames = list(enames, NULL))
##' prune_embeddings(v2, emat)
##' prune_embeddings(v2, t(emat)) # automatic detection of the orientation
##'
##' vembs <- prune_embeddings(v2, emat)
##' all(vembs[enames, ] == emat[enames, ])
##'
##' @export
prune_embeddings <- function(vocab, embeddings,
                     nbuckets = attr(vocab, "nbuckets"),
                     max_in_bucket = 30) {
  if (is.null(colnames(embeddings)) && is.null(rownames(embeddings)))
    stop("Terms dimension of `embeddings` must be named")
  by_row <-
    if (!is.null(rownames(embeddings)))
      is.null(colnames(embeddings)) || nrow(embeddings) > ncol(embeddings)
    else FALSE
  out <- C_prune_embeddings(vocab, embeddings, by_row, nbuckets, max_in_bucket)
  out
}
