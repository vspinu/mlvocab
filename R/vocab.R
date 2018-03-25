
`_empty_vocab` <- structure(data.frame(term = character(), term_count = integer(), doc_count = integer()),
                            document_count = 0,
                            sep_ngram = " ",
                            ngram = c(1L, 1L),
                            chargram = FALSE)

##' Manipulate vocabularies
##'
##' [vocab()] creates a vocabulry from a text corpus. [update_vocab()] updates
##' an existing vocabulary. [prune_vocab()] prunes vocabulary.  
##' @param corpus list of character vectors
##' @param ngram a vector of the form `c(min_ngram, max_ngram)`.
##' @param chargram currently unused
##' @export
vocab <- function(corpus, ngram = c(1, 1), chargram = FALSE) {
    old_vocab <- structure(`_empty_vocab`,
                           ngram = as.integer(ngram),
                           chargram = as.logical(chargram))
    C_vocab(corpus, old_vocab)
}

##' @param vocab `data.frame` obtained from a call to [vocab()] or
##'     [text2vec::create_vocab()] from `text2vec` package.
##' @rdname vocab
##' @export
update_vocab <- function(vocab, corpus) {
    if (!inherits(vocab, c("mlvocab_vocab", "text2vec_vocabulary")))
        stop("'vocab' must be of class 'mlvocab_vocab' or 'text2vec_vocabulary'")
    ## if (isTRUE(attr(vocab, "chargram", F)))
    ##     attr(vocab, "chargram") <- FALSE
    C_vocab(corpus, vocab)
}

##'
##' `prune_vocab` is an adaptation of [text2vec::prune_vocabulary()].
##' @param max_terms max number of terms to preserve
##' @param term_count_min keep terms occurring at _least_ this many times over all docs
##' @param term_count_max keep terms occurring at _most_ this many times over all docs
##' @param doc_count_min,doc_proportion_min keep terms appearing in at _least_ this many docs
##' @param doc_count_max,doc_proportion_max keep terms appearing in at _most_ this many docs
##' @rdname vocab
##' @export
prune_vocab <- function(vocab,
                        max_terms = Inf, 
                        term_count_min = 1L,
                        term_count_max = Inf,
                        doc_proportion_min = 0.0,
                        doc_proportion_max = 1.0,
                        doc_count_min = 1L,
                        doc_count_max = Inf) {

    if (!inherits(vocab, "text2vec_vocabulary"))
        stop("'vocab' should be an object of class text2vec_vocabulary")

    vocab_size <- nrow(vocab)
    document_count <- attr(vocab, "document_count", TRUE)

    ind <- TRUE

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
    out <- vocab[ind, ]

    if (is.finite(max_terms) && nrow(out) > max_terms) {
        rnk <- rank(-out[["term_count"]])
        out <- out[rnk <= max_terms, ]
    }

    row.names(out) <- NULL
    for (a in setdiff(names(attributes(out)), "row.names")) {
        attr(out, a) <- attr(vocab, a, TRUE)
    }
    out
}

##'
##'
##' [embed_vocab()] is commonly used in conjunction with sequence generators
##' ([text2ixmat()] and [text2ixseq()]). When a terms in a corpus is not present
##' in a vocabulary (aka unknown), it is hashed into one of the
##' `unknown_buckets` (hashing trick). Embeddings hashed into a bucket are
##' averaged to produce the embedding for that bucket.
##'
##' Similarly, when a term from the vocabulary is not present in the embedding
##' matrix (aka missing) `min_to_average` embeddings are averaged to produce the
##' missing embedding. Different buckets are used for "missing" and "unknown"
##' embeddings because `unknown_buckets` can be 0.
##' 
##' @param embeddings embeddings matrix. The terms dimension must be named. If
##'     both [colnames()] and [rownames()] are non-null, dimension with more
##'     elements is considered term-dimension.
##' @param unknown_buckets How many unknown buckets to create along the terms of
##'     the `vocab`. For each unknown bucket at least `max_in_bucket` embedding
##'     vectors will be hashed into the bucket and averaged.
##' @param max_in_bucket At most this many embedding vectors will be averaged
##'     into each unknown or missing bucket (see details). Lower number results
##'     in faster processing. For large `unknown_buckets` this number might not
##'     be reached due to the finiteness of the `embeddings` vocabulary, or even
##'     result in `0` embeddings being hashed into a bucket producing `[0 0
##'     ...]` embeddings for some buckets.
##' @rdname vocab
##' @export
embed_vocab <- function(vocab, embeddings, unknown_buckets = 0, max_in_bucket = 30) {
    if (is.null(colnames(embeddings)) && is.null(rownames(embeddings)))
        stop("Terms dimension of `embeddings` must be named")
    by_row <-
        if (!is.null(rownames(embeddings)))
            is.null(colnames(embeddings)) || nrow(embeddings) > ncol(embeddings)
        else FALSE
    out <- C_embed_vocab(vocab, embeddings, by_row, unknown_buckets, max_in_bucket)
    ## names <- vocab$term
    ## if (unknown_buckets > 0)
    ##     names <- c(names, paste0("bkt", seq_len(unknown_buckets)))
    ## if (by_row) rownames(out) <- names
    ## else colnames(out) <- names
    out
}


### OTHERS

##' @export
mlvocab <- function(x = identity, corpus_var, ngram = c(1, 1), chargram = FALSE,
                    vocab_name = corpus_var, ...) {
    if (is.function(x))
        return(mlfunction("mlvocab"))
    mlcontinue(switch(x$op,
                      describe = assoc(x, c("describe", "mlvocab"),
                                       ll(doc = "Compute vocabulary from `vocab_corpus`.", 
                                          handles = c("run", "describe"))), 
                      run =
                          assoc(x, c("vocab", vocab_name), {
                              old_vocab <- x[["vocab"]][[vocab_name]]
                              corpus <- x[["data"]][[corpus_var]]
                              if (is.null(old_vocab))
                                  vocab(corpus = corpus, ngram = ngram, chargram = chargram)
                              else
                                  update_vocab(old_vocab, corpus)
                          }), 
                      x))
}

#' @export
#' @method print mlvocab_vocab
print.mlvocab_vocab <- function(x, ...) {
    cat("Number of docs: ", attr(x, "document_count", TRUE), "\n",
        "Ngrams: ", paste(attr(x, "ngram", TRUE), collapse = " "), "\n",
        "Vocabulary:\n", sep = "")
    newx <- x
    oldClass(newx) <- "data.frame"
    if (nrow(newx) > 20) {
        newx <- rbind(format.data.frame(head(newx)),
                      data.frame(term = "...", term_count = "...", doc_count = "...", row.names = ""),
                      format.data.frame(tail(newx)))
    }
    print(newx)
    invisible(x)
}

#' @keywords internal
#' @export
arrange.mlvocab_vocab <- function(.data, ...) {
    out <- NextMethod("arrange")
    oldClass(out) <- class(.data)
    out
}
