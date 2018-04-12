
`_empty_vocab` <- structure(data.frame(term = character(), term_count = integer(), doc_count = integer(), stringsAsFactors = F),
                            document_count = 0L,
                            nbuckets = 0L, 
                            ngram_sep = "_",
                            ngram = c(1L, 1L))

.normalize_ngram <- function(ngram) {
  if (length(ngram) == 1L) c(1L, as.integer(ngram))
  else ngram
}

##' Build and manipulate vocabularies
##'
##' [vocab()] creates a vocabulry from a text corpus; [vocab_update()] and
##' [vocab_prune()], respectively,  update and prune an existing vocabulary.
##' @param corpus list of character vectors
##' @param ngram a vector of length 2 of the form `c(min_ngram, max_ngram)` or a
##'   singleton `max_ngram` which is equivalent to `c(1L, max_ngram)`.
##' @param ngram_sep separator to link terms within ngrams.
##' @examples
##' corpus <-
##'    list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"), 
##'         b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
##'               "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"))
##'
##' vocab(corpus)
##' vocab(corpus, ngram = 3)
##' vocab(corpus, ngram = c(2, 3))
##'
##' v <- vocab(corpus)
##'
##' extra_corpus <- list(extras = c("apples", "oranges"))
##' v <- vocab_update(v, extra_corpus)
##' v
##'
##' vocab_prune(v, max_terms = 7)
##' vocab_prune(v, term_count_min = 2)
##' vocab_prune(v, max_terms = 7, nbuckets = 2)
##'
##' v2 <- vocab_prune(v, max_terms = 7, nbuckets = 2)
##' enames <- c("the", "quick", "brown", "fox", "jumps")
##' emat <- matrix(rnorm(50), nrow = 5,
##'                dimnames = list(enames, NULL))
##'
##' vocab_embed(v2, emat)
##' vocab_embed(v2, t(emat)) # automatic detection of the orientation
##'
##' vembs <- vocab_embed(v2, emat)
##' all(vembs[enames, ] == emat[enames, ])
##' @export
vocab <- function(corpus, ngram = c(1, 1), ngram_sep = "_") {
  old_vocab <- structure(`_empty_vocab`,
                         ngram = .normalize_ngram(ngram),
                         ngram_sep = ngram_sep)
  C_vocab(corpus, old_vocab)
}

##' @param vocab `data.frame` obtained from a call to [vocab()].
##' @name vocab
##' @export
vocab_update <- function(vocab, corpus) {
  if (!inherits(vocab, "mlvocab_vocab"))
    stop("'vocab' must be of class 'mlvocab_vocab'")
  pruned <- attr(vocab, "pruned")
  if (isTRUE(attr(vocab, "pruned"))) {
    stop("Cannot update pruned vocabulary")
  }
  ## if (isTRUE(attr(vocab, "chargram", F)))
  ##     attr(vocab, "chargram") <- FALSE
  C_vocab(corpus, vocab)
}

##' @param max_terms max number of terms to preserve
##' @param term_count_min keep terms occurring at _least_ this many times over
##'   all docs
##' @param term_count_max keep terms occurring at _most_ this many times over
##'   all docs
##' @param doc_count_min,doc_proportion_min keep terms appearing in at _least_
##'   this many docs
##' @param doc_count_max,doc_proportion_max keep terms appearing in at _most_
##'   this many docs
##' @param nbuckets How many unknown buckets to create along the
##'   remaining terms of the pruned `vocab`. All pruned terms will be hashed
##'   into this many buckets and the corresponding statistics (`term_count` and
##'   `doc_count`) updated.
##' @name vocab
##' @export
vocab_prune <- function(vocab,
                        max_terms = Inf, 
                        term_count_min = 1L,
                        term_count_max = Inf,
                        doc_proportion_min = 0.0,
                        doc_proportion_max = 1.0,
                        doc_count_min = 1L,
                        doc_count_max = Inf,
                        nbuckets = attr(vocab, "nbuckets")) {

  ## adapted from [text2vec::vocab_pruneulary()]
  
  if (!inherits(vocab, "mlvocab_vocab"))
    stop("'vocab' must be an object of class `mlvocab_vocab`")

  ubkts_old <- attr(vocab, "nbuckets")
  if (ubkts_old > 0 && ubkts_old != nbuckets)
    stop("Cannot rehash current unknown buckets with a different value of `nbuckets`")

  document_count <- attr(vocab, "document_count", TRUE)
  ind <- !grepl("^__", vocab$term)

  if (term_count_min > 1L)
    ind <- ind & (vocab[["term_count"]] >= term_count_min)
  if (is.finite(term_count_max))
    ind <- ind & (vocab[["term_count"]] <= term_count_max)

  if (doc_count_min > 1L)
    ind <- ind & (vocab[["doc_count"]] >= doc_count_min)
  if (is.finite(doc_count_max))
    ind <- ind & (vocab[["doc_count"]] <= doc_count_max)

  doc_proportion <- NULL
  if (doc_proportion_min > 0) {
    doc_proportion <- vocab[["doc_count"]] / document_count
    ind <- ind & (doc_proportion >= doc_proportion_min)
  }
  if (doc_proportion_max < 1.0) {
    if (is.null(doc_proportion))
      doc_proportion <- vocab[["doc_count"]] / document_count
    ind <- ind & (doc_proportion <= doc_proportion_max)
  }

  ## fixme: define custom [ which drops row.names
  pruned <- vocab[ind, ]

  if (is.finite(max_terms) && nrow(pruned) > max_terms) {
    rnk <- rank(-pruned[["term_count"]], ties.method = "first")
    pruned <- pruned[rnk <= max_terms, ]
  }

  row.names(pruned) <- NULL
  for (a in setdiff(names(attributes(pruned)), "row.names")) {
    attr(pruned, a) <- attr(vocab, a, TRUE)
  }

  attr(pruned, "nbuckets") <- 0L

  if  (nbuckets > 0) {
    pruned <- C_rehash_vocab(pruned, vocab, nbuckets)
  }

  attr(pruned, "pruned") <- TRUE
  pruned
}

##' @description [vocab_embed()] subsets a (commonly large) pre-trained
##'   word-vector matrix into a smaller, one vector per term, embedding
##'   matrix.
##'
##' [vocab_embed()] is commonly used in conjunction with sequence generators
##' ([timat()] and [tiseq()]). When a term in a corpus is not present in a
##' vocabulary (aka unknown), it is hashed into one of the `nbuckets`
##' buckets. Embeddings which are hashed into same bucket are averaged to
##' produce the embedding for that bucket. Maximum number of embeddings to
##' average per bucket is controled with `max_in_bucket` parameter.
##'
##' Similarly, when a term from the vocabulary is not present in the embedding
##' matrix (aka missing) `max_in_bucket` embeddings are averaged to produce the
##' missing embedding. Different buckets are used for "missing" and "unknown"
##' embeddings because `nbuckets` can be 0.
##' 
##' @param embeddings embeddings matrix. The terms dimension must be named. If
##'   both [colnames()] and [rownames()] are non-null, dimension with more
##'   elements is considered term-dimension.
##' @param max_in_bucket At most this many embedding vectors will be averaged
##'   into each unknown or missing bucket (see details). Lower number results in
##'   faster processing. For large `nbuckets` this number might not be
##'   reached due to the finiteness of the `embeddings` vocabulary, or even
##'   result in `0` embeddings being hashed into a bucket producing `[0 0 ...]`
##'   embeddings for some buckets.
##' @name vocab
##' @export
vocab_embed <- function(vocab, embeddings,
                        nbuckets = attr(vocab, "nbuckets"),
                        max_in_bucket = 30) {
  if (is.null(colnames(embeddings)) && is.null(rownames(embeddings)))
    stop("Terms dimension of `embeddings` must be named")
  by_row <-
    if (!is.null(rownames(embeddings)))
      is.null(colnames(embeddings)) || nrow(embeddings) > ncol(embeddings)
  else FALSE
  out <- C_embed_vocab(vocab, embeddings, by_row, nbuckets, max_in_bucket)
  out
}


### OTHER STUFF

## mlvocab <- function(x = identity, corpus_var, ngram = c(1, 1), 
##                     vocab_name = corpus_var, ...) {
##   if (is.function(x))
##     return(mlfunction("mlvocab"))
##   mlcontinue(switch(x$op,
##                     describe = assoc(x, c("describe", "mlvocab"),
##                                      ll(doc = "Compute vocabulary from `vocab_corpus`.", 
##                                         handles = c("run", "describe"))), 
##                     run =
##                       assoc(x, c("vocab", vocab_name), {
##                         old_vocab <- x[["vocab"]][[vocab_name]]
##                         corpus <- x[["data"]][[corpus_var]]
##                         if (is.null(old_vocab))
##                           vocab(corpus = corpus, ngram = ngram)
##                         else
##                           vocab_update(old_vocab, corpus)
##                       }), 
##                     x))
## }

#' @export
#' @method print mlvocab_vocab
print.mlvocab_vocab <- function(x, ...) {
  cat("Number of docs: ", attr(x, "document_count", TRUE), "\n",
      "Ngrams: ", paste(attr(x, "ngram", TRUE), collapse = " "), "\n",
      "Buckets: ", attr(x, "nbuckets"), "\n", 
      if (isTRUE(attr(x, "pruned", TRUE))) "Pruned vocabulary:" else "Vocabulary", "\n",
      sep = "")
  newx <- x
  oldClass(newx) <- "data.frame"
  nbuckets <- attr(x, "nbuckets")
  if (nrow(newx) > 30) {
    divider <- data.frame(term = "...", term_count = "...", doc_count = "...", row.names = "")
    tail_size <- if (nbuckets > 20) 6 else nbuckets + 6
    newx <- rbind(format.data.frame(head(newx)),
                  if (nbuckets > 20) {
                    ustart <- nrow(x) - nbuckets
                    urange <- (ustart - 5):(ustart + 6)
                    divider2 <- data.frame(term = "...", term_count = "...", doc_count = "...", row.names = " ")
                    rbind(divider,
                          format.data.frame(newx[urange, ]),
                          divider2)
                  } else {
                    divider
                  },
                  format.data.frame(tail(newx, n = tail_size)))
    rownames(newx) <- sub("^  .*$", "", rownames(newx))
  }
  print(newx)
  invisible(x)
}

#' Methods for `dplyr` predicates
#'
#' Needed to circumvent dropping attributes by dplyr/tibble functions.
#'
#' @param .data vocab data.frame
#' @param ... other parameters
#' @name dplyr_methods
#' @keywords internal
#' @export
arrange.mlvocab_vocab <- function(.data, ...) {
  out <- NextMethod("arrange")
  oldClass(out) <- class(.data)
  out
}
