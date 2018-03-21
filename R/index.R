

##' Convert text to integer indexes
##'
##' @rdname term_index
##' @param corpus 
##' @param vocab 
##' @param keep_unknown 
##' @param unknown_buckets 
##' @export
text2ixseq <- function(corpus, vocab, keep_unknown = unknown_buckets > 0, unknown_buckets = 0,
                         reverse = FALSE) {
    structure(C_corpus2ixseq(corpus, vocab,  keep_unknown, unknown_buckets, reverse),
              names = names(corpus))
}

##' @rdname term_index
##' @param corpus 
##' @param vocab 
##' @param maxlen 
##' @param pad_right 
##' @param trunc_right 
##' @param keep_unknown 
##' @export
text2ixmat <- function(corpus, vocab, maxlen = 100, pad_right = TRUE, trunc_right = TRUE,
                       keep_unknown = unknown_buckets > 0,  unknown_buckets = 0, reverse = FALSE) {
    out <- C_corpus2ixmat(corpus, vocab, maxlen, pad_right, trunc_right,
                          keep_unknown, unknown_buckets, reverse)
    if (!is.null(names(corpus)))
        rownames(out) <- names(corpus)
    out
}
