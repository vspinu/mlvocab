##' Vocabulary preprocessing utilities for nlp pipelines.
##'
##' The following consistent abstraction is provided by the `mlvocab`
##' package. First, the vocabulary object should be built from the entire corpus
##' with the help of [vocab()], [vocab_update()] and [vocab_prune()]
##' functions. Then, the vocabulary is passed alongside the corpus to a variety
##' of corpus pre-processing functions.
##'
##' Most of the `mlvocab` functions accept `unknown_buckets` argument for
##' partial or full hashing of the corpus.
##' 
##' Current functionality includes:
##' 
##' \itemize{
##'
##' \item{term index sequences}{[tixseq()] and [ttixmat()] produce integer
##'   sequences suitable for direct consumption by various sequence models.}
##'
##' \item{term matrices}{[dtm()], [tdm()] and [tcm()] create document-term,
##' term-document and term-co-occurrence matrices respectively.}
##'
##' \item{vocabulary embedding}{given pre-trained word-vectors [embed_vocab()]
##' creates smaller embedding matrices treating missing and unknown vocabulary
##' terms with grace.}
##'
##' \item{tfidf weighting}{[tfidf()] copters various versions of term frequency,
##' inverse document frequency weighting of `dtm` and `tdm` matrices.}
##' 
##' }
##'
##' @author Vitalie Spinu (\email{spinuvit@gmail.com})
##' @importFrom digest digest
##' @importFrom Rcpp sourceCpp
##' @importFrom Matrix Diagonal t rowSums colSums
##' @useDynLib mlvocab, .registration=TRUE
##' @keywords internal
"_PACKAGE"
