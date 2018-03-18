
##' Manipulate vocabularies
##'
##' [vocab()] creates a vocabulry from a text corpus. [vocab_update()] updates
##' an existing vocabulary. [vocab_prune()] prunes vocabulary.  
##' @param corpus list of character vectors
##' @param ngram a vector of the form `c(min_ngram, max_ngram)`.
##' @param chargram currently unused
##' @export
vocab <- function(corpus, ngram = c(1, 1), chargram = FALSE) {
    old_vocab <-
        structure(data.frame(term = character(), term_count = integer(), doc_count = integer()),
                  document_count = 0, sep_ngram = " ", ngram = as.integer(ngram),
                  chargram = as.logical(chargram))
    C_vocab(corpus, old_vocab)
}

##' @param vocab `data.frame` obtained from a call to [vocab()] or
##'     [text2vec::create_vocab()] from `text2vec` package.
##' @rdname vocab
##' @export
vocab_update <- function(vocab, corpus) {
    if (!inherits(vocab, c("mlvocab_vocab", "text2vec_vocabulary")))
        stop("'vocab' must be of class 'mlvocab_vocab' or 'text2vec_vocabulary'")
    ## if (isTRUE(attr(vocab, "chargram", F)))
    ##     attr(vocab, "chargram") <- FALSE
    C_vocab(corpus, vocab)
}

##'
##' `vocab_prune` is an adaptation of [text2vec::prune_vocabulary()].
##' @param max_terms max number of terms to preserve
##' @param term_count_min keep terms occurring at _least_ this many times over all docs
##' @param term_count_max keep terms occurring at _most_ this many times over all docs
##' @param doc_count_min,doc_proportion_min keep terms appearing in at _least_ this many docs
##' @param doc_count_max,doc_proportion_max keep terms appearing in at _most_ this many docs
##' @rdname vocab
##' @export
vocab_prune <- function(vocab,
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

    out <- vocab[ind, ]

    if (is.finite(max_terms) && nrow(out) > max_terms) {
        ord <- order(out[["term_count"]], decreasing = TRUE)
        out <- out[ord[ord <= max_terms]]
    }

    attributes(out) <- modifyList(attributes(vocab), list(row.names = NULL))
    out
}

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
                                  vocab_update(old_vocab, corpus)
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
