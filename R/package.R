##' `mlvocab` package
##'
##' The following two-step abstraction is provided by the `mlvocab`
##' package. First, the vocabulary object is built from the entire corpus with
##' the help of [vocab()], [update_vocab()] and [prune_vocab()]
##' functions. Second, the vocabulary is passed alongside the corpus to a
##' variety of corpus pre-processing functions.
##'
##' Most of the `mlvocab` functions accept `nbuckets` argument for
##' partial or full hashing of the corpus.
##' 
##' Current functionality includes:
##' 
##' \itemize{
##'
##' \item{term index sequences}{[tix_seq()] and [tix_mat()] produce integer
##'   sequences suitable for direct consumption by various sequence models.}
##'
##' \item{term matrices}{[dtm()], [tdm()] and [tcm()] create document-term,
##' term-document and term-co-occurrence matrices respectively.}
##'
##' \item{vocabulary embedding}{given pre-trained word-vectors [prune_embeddings()]
##' creates smaller embedding matrices treating missing and unknown vocabulary
##' terms with grace.}
##'
##' \item{tfidf weighting}{[tfidf()] computes various versions of term
##' frequency, inverse document frequency weighting of `dtm` and `tdm`
##' matrices.}
##' 
##' }
##'
##' @author Vitalie Spinu (\email{spinuvit@gmail.com})
##' @import sparsepp
##' @importFrom digest digest
##' @importFrom Rcpp sourceCpp
##' @importFrom Matrix Diagonal t rowSums colSums
##' @importFrom methods new
##' @importFrom utils head tail
##' @useDynLib mlvocab, .registration=TRUE
"_PACKAGE"
