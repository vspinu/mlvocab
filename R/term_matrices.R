##' Term-document and term-cooccurrence matrices
##'
##' These functions compute or update various term-counts of a corpus with flexible output
##' specification.
##'
##' @param corpus text corpus; see `[vocab()]`.
##' @param vocab a `data.frame` produced by an early call to [vocab()]. When
##'   `vocab` is `NULL` and `nbuckets` is `NULL` or `0`, the vocabulary is first
##'   computed from corpus. When `nbuckets` > `0` and `vocab` is `NULL` the
##'   result matrix will consist of buckets only.
##' @param nbuckets number of unknown buckets
##' @param output one of "triplet", "column", "row", "df" or an unambiguous
##'   abbreviation thereof. First three options return the corresponding sparse
##'   matrices from Matrix package, "df" results in a triplet
##'   `data.frame`.
##'
##'   The default output type corresponds to the most efficient computation in
##'   terms of CPU and memory usage ("row" for `dtm`, "column" for `tdm` and
##'   "triplet" for `tcm`), but benefits are marginal unless your matrices are
##'   so big that they barely fit into memory. If you plan to further perform
##'   matrix algebra on these matrices it's a good idea to choose "column" type
##'   because of the much better support from the Matrix package.
##' @name term_matrices
##' @export
dtm <- function(corpus, vocab = NULL,
                ngram = attr(vocab, "ngram"),
                nbuckets = attr(vocab, "nbuckets"),
                output = c("row", "triplet", "column", "df")) {
  tm(C_dtm, corpus, vocab, ngram, nbuckets, output)
}

##' @rdname term_matrices
##' @export
tdm <- function(corpus, vocab = NULL,
                ngram = attr(vocab, "ngram"),
                nbuckets = attr(vocab, "nbuckets"),
                output = c("column", "triplet", "row", "df")) {
  tm(C_tdm, corpus, vocab, ngram, nbuckets, output)
}

##' @details
##' For `ngram_max > 1` the weights vectors is automatically extended to match
##' the "imaginary" sliding window over the ngrams. A proximity weight attached
##' for an n-gram is an average of weights of the constituents of the ngram in
##' the original sequence. Such scheme results in a consistent weighting across
##' different values of `ngram_min` and `ngram_max`, and it is the reason why
##' first element of `window_weights` is the proximity to the context word
##' itself (i.e. distance `0`). For example:
##'
##' \itemize{
##'   \item default weights for the context window `["a" "b" "c" "d" "e"]`
##'      \tabular{rrrrr}{
##'         a\tab b\tab c\tab d\tab e\cr
##'      1.00\tab 0.50\tab 0.33\tab 0.25\tab 0.20
##'      }
##'   \item for `ngram=c(1L, 3L)`
##'      \tabular{rrrrrrrrrrrr}{
##'      a \tab a_b \tab a_b_c \tab b \tab b_c \tab b_c_d \tab c \tab c_d \tab c_d_e \tab d \tab d_e \tab e \cr
##'      1.00\tab 0.75\tab 0.61\tab 0.50\tab 0.42\tab 0.36\tab 0.33\tab 0.29\tab 0.26\tab 0.25\tab 0.22\tab 0.20 \cr
##'      }
##'   \item for `ngram=c(2L, 3L)`
##'      \tabular{rrrrrrr}{
##'      a_b \tab a_b_c \tab b_c \tab b_c_d \tab c_d \tab c_d_e \tab d_e \cr
##'      0.75\tab 0.61\tab 0.42\tab 0.36\tab 0.29\tab 0.26\tab 0.22
##'      }
##' }
##'
##' @param window_size sliding window size used for co-occurrence
##'   computation. In this implementation the window includes the context word;
##'   thus, window_size == 1 will result in 0 co-occurrence matrix. This
##'   convention allows for consistent weighting schemes across different values
##'   of `ngram_min` and `ngram_max`.
##' @param window_weights vector of weights which are superimposed on the
##'   sliding `window`. First element is a weight for distance 0 (aka context
##'   word itself),  second for distance 1 etc. First weight is ignored for
##'   `ngram_max` == 1, see details. `window_weights` is recycled to length
##'   `window_size` if needed. It can be a string naming a function or a
##'   function which accepts one argument, `window_size`,  and returns a
##'   `window_weights` vector. Defaults to `[1, 1/2, ..., 1/window_size]`.
##' @param context when "symmetric", matrix entries `(i, j)` and `(j, i)` are
##'   the same and represent coocurence of terms `i` and `j` within
##'   `window_size`. When "right", entry `(i, j)` represents coocurence of the
##'   term `j` on the right side of `i`. When "left", entry `(i, j) represents
##'   the coocurence of the term `j` on the left of term `i`.
##' @param ngram an integer vector of the form `[ngram_min,
##'   ngram_max]`. Defaults to the `ngram` settings used during the creation of
##'   `vocab`. Explicitly providing this parameter should rarely be needed.
##' @rdname term_matrices
##' @export
tcm <- function(corpus, vocab = NULL,
                window_size = 5,
                window_weights = 1/seq.int(window_size),
                context = c("symmetric", "right", "left"),
                ngram = attr(vocab, "ngram"),
                nbuckets = attr(vocab, "nbuckets"),
                output = c("triplet", "column", "row", "df")) {
  if (is.character(window_weights))
    match.fun(window_weights)
  if (is.function(window_weights))
    window_weights <- do.call(window_weights, list(window_size))
  window_weights <- rep_len(window_weights, window_size)
  context <- match.arg(context)
  if (is.null(ngram))
    ngram <- c(1L, 1L)
  ## DO NOT DELETE!!
  ## term_weights <- retrive_weights(term_weights, vocab)
  ## if (!is.null(term_weights))
  ##   term_weights <- sqrt(term_weights)
  tm(C_tcm, corpus, vocab, ngram, nbuckets, output,
     window_size, window_weights, context)
}

tm <- function(cfun, corpus, vocab, ngram, nbuckets, output, ...) {
  output <- match.arg(output[[1]], c("triplet", "column", "row", "df"))
  if (is.null(vocab)) {
    if (is.null(nbuckets) || nbuckets == 0) {
      vocab <- vocab(corpus)
    } else {
      vocab <- `_empty_vocab`
    }
  }
  if (is.null(nbuckets))
    nbuckets <- attr(vocab, "nbuckets")
  if (is.null(ngram))
    ngram <- attr(vocab, "ngram")
  do.call(cfun, list(corpus, vocab, NULL, nbuckets, output, ...,
                     ngram_min = ngram[[1]], ngram_max = ngram[[2]]))
}

ngram_weights <- function(weights = 1/seq_len(5), ngram_min = 1, ngram_max = 3) {
  stopifnot(length(weights) < (length(letters) - ngram_max))
  n <- length(weights)
  w <- C_ngram_weights(weights, ngram_min, ngram_max)
  names <- C_wordgram(letters[1:(n + ngram_max)], ngram_min, ngram_max, "_")
  structure(w, names = names[1:length(w)])
}


### DO NOT DELETE!! maybe use some day
## retrive_weights <- function(term_weights, vocab) {
##   if (!is.null(term_weights)) {
##     if (is.character(term_weights)) {
##       if (!is.null(vocab[[term_weights]]))
##         term_weights <- vocab[[term_weights]]
##       else
##         term_weights <- match.fun(term_weights)
##     }
##     if (is.function(term_weights))
##       term_weights <- do.call(term_weights, list(vocab))
##     if (is.numeric(term_weights)) {
##       stopifnot(length(term_weights) == nrow(vocab))
##       return(term_weights)
##     }
##     stop("Non numeric `term_weights` supplied")
##   }
## }
