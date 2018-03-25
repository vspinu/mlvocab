

##' Term and co-occurrence matrices
##'
##' These functions compute various term-counts of a corpus with flexible output
##' specification.
##' 
##' @param corpus a list of character vectors
##' @param vocab a `data.frame` produced by an early call to [vocab()]. When
##'   `vocab` is `NULL` and `unknown_buckets` is `NULL` or `0`, the vocabulary
##'   is first computed from corpus. When `unknown_buckets` > `0` and `vocab` is
##'   `NULL` the result matrix will consist of buckets only.
##' @param unknown_buckets number of unknown buckets
##' @param output one of "triplet", "column", "row", "df" or an unambiguous
##'   abbreviation thereof. First three options return the corresponding sparse
##'   matrices from Matrix package, "df" results in a triplet `data.frame`.
##' @rdname term_matrices
##' @export
dtm <- function(corpus, vocab = NULL,
                unknown_buckets = attr(vocab, "unknown_buckets"),
                output = c("triplet", "column", "row", "df")) {
  tm(C_dtm, corpus, vocab, unknown_buckets, output)
}

##' @rdname term_matrices
##' @export
tdm <- function(corpus, vocab = NULL,
                unknown_buckets = attr(vocab, "unknown_buckets"),
                output = c("triplet", "column", "row", "df")) {
  tm(C_tdm, corpus, vocab, unknown_buckets, output)
}

tm <- function(cfun, corpus, vocab, unknown_buckets, output) {
  
  output <- match.arg(output, c("triplet", "column", "row", "df"))
  if (is.null(vocab)) {
    if (is.null(unknown_buckets) || unknown_buckets == 0) {
      vocab <- vocab(corpus)
    } else {
      vocab <- `_empty_vocab`
    }
  }

  if (is.null(unknown_buckets))
    unknown_buckets <- 0L
  
  do.call(cfun, list(corpus, vocab, unknown_buckets, output))
}
