
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
##' [vocab()] creates a vocabulary from a text corpus; [update_vocab()] and
##' [prune_vocab()] update and prune an existing vocabulary respectively.
##'
##' When `corpus` is a character vector each string is tokenized with `regex`
##' with the internal tokenizer. When `corpus` has names, names will be used to
##' name the output whenever appropriate.
##'
##' When corpus is a `data.frame`, the documents must be in last column,  which
##' can be either a list of strings or a character vector. All other columns are
##' considered document ids. If first column is a character vector most function
##' will use it to name the output.
##'
##' @param corpus A collection of ASCII or UTF-8 encoded documents. It can be a
##'   list of character vectors, a character vector or a data.frame with at
##'   least two columns - id and documents. See details.
##' @param ngram a vector of length 2 of the form `c(min_ngram, max_ngram)` or a
##'   singleton `max_ngram` which is equivalent to `c(1L, max_ngram)`.
##' @param ngram_sep separator to link terms within ngrams.
##' @param regex a regexp to be used for segmentation of documents when `corpus`
##'   is a character vector; ignored otherwise. Defaults to a set of basic white
##'   space separators. `NULL` means no segmentation. The regexp grammar is the
##'   extended ECMAScript as implemented in C++11.
##'
##' @references
##'
##' https://en.cppreference.com/w/cpp/regex/ecmascript
##'
##' @examples
##'
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
##' v <- update_vocab(v, extra_corpus)
##' v
##'
##' prune_vocab(v, max_terms = 7)
##' prune_vocab(v, term_count_min = 2)
##' prune_vocab(v, max_terms = 7, nbuckets = 2)
##'
##' @export
vocab <- function(corpus, ngram = c(1, 1), ngram_sep = "_", regex = "[[:space:]]+") {
  old_vocab <- structure(`_empty_vocab`,
                         ngram = .normalize_ngram(ngram),
                         ngram_sep = ngram_sep,
                         regex = regex)
  C_vocab(corpus, old_vocab)
}

##' @param vocab `data.frame` obtained from a call to [vocab()].
##' @name vocab
##' @export
update_vocab <- function(vocab, corpus) {
  if (!inherits(vocab, "mlvocab_vocab"))
    stop("'vocab' must be of class 'mlvocab_vocab'")
  if (isTRUE(attr(vocab, "pruned"))) {
    ## Updating would makes sense if nbuckets > 0 but original prune criteria
    ## will be violated. So it doesn't seem worth supporting.
    stop("Cannot update pruned vocabulary")
  }
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
##' @param nbuckets How many unknown buckets to create along the remaining terms
##'   of the pruned `vocab`. All pruned terms will be hashed into this many
##'   buckets and the corresponding statistics (`term_count` and `doc_count`)
##'   updated.
##' @name vocab
##' @export
prune_vocab <- function(vocab,
                        max_terms = Inf,
                        term_count_min = 1L,
                        term_count_max = Inf,
                        doc_proportion_min = 0.0,
                        doc_proportion_max = 1.0,
                        doc_count_min = 1L,
                        doc_count_max = Inf,
                        nbuckets = attr(vocab, "nbuckets")) {

  ## adapted from [text2vec::prune_vocabulary()]

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
##                           update_vocab(old_vocab, corpus)
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
