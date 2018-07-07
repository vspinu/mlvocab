 
#include "vocab.h"
 
// [[Rcpp::export]]
DataFrame C_vocab(SEXP corpus0, const DataFrame& oldvocab) {
  Vocab* vocab = new Vocab(oldvocab);
  Corpus corpus(corpus0, vocab->separators());
  vocab->insert_corpus(corpus);
  return vocab->df();
}
 
// [[Rcpp::export]]
NumericMatrix C_embed_vocab(const DataFrame& vocabdf, NumericMatrix& embeddings, bool by_row,
                            int nbuckets, int min_to_average) {
  Vocab* v = new Vocab(vocabdf);
  return(v->embed_vocab(embeddings, by_row, nbuckets, min_to_average));
}

// [[Rcpp::export]]
DataFrame C_rehash_vocab(const DataFrame& pruned_vocabdf, const DataFrame& vocabdf, const int nbuckets) {
  Vocab* v = new Vocab(pruned_vocabdf);
  v->rebucket_unknowns(vocabdf, nbuckets);
  return v->df();
}

// [[Rcpp::export]]
SEXP C_corpus2ixseq(SEXP corpus0, const DataFrame& vocabdf,
                    bool keep_unknown, int nbuckets, bool reverse) {
  Vocab* v = new Vocab(vocabdf);
  Corpus corpus(corpus0, v->separators());
  return(v->corpus2ixseq(corpus, keep_unknown, nbuckets, reverse));
}

// [[Rcpp::export]]
DataFrame C_corpus2ixdf(SEXP corpus0, const DataFrame& vocabdf,
                        bool keep_unknown, int nbuckets, bool reverse) {
  Vocab* v = new Vocab(vocabdf);
  Corpus corpus(corpus0, v->separators());
  return(v->corpus2ixdf(corpus, keep_unknown, nbuckets, reverse));
}

// [[Rcpp::export]]
IntegerMatrix C_corpus2ixmat(SEXP corpus0, const DataFrame& vocabdf,
                             int maxlen, bool pad_right, bool trunc_right,
                             bool keep_unknown, int nbuckets, bool reverse) {
  Vocab* v = new Vocab(vocabdf);
  const Corpus corpus(corpus0, v->separators());
  IntegerMatrix out = v->corpus2ixmat(corpus, maxlen, pad_right, trunc_right, keep_unknown, nbuckets, reverse);
  rownames(out) = corpus.names();
  return(out);
}
 
// [[Rcpp::export]]
SEXP C_dtm(SEXP corpus0, const DataFrame& vocabdf,
           const Nullable<NumericVector>& term_weights, const int nbuckets,
           const std::string output,
           const int ngram_min, const int ngram_max) {
  Vocab* v = new Vocab(vocabdf);
  Corpus corpus(corpus0, v->separators());
  if (output == "triplet") {
    return v->term_matrix<MatrixType::DGT>(corpus, nbuckets, true, ngram_min, ngram_max, term_weights);
  } else if (output == "column") {
    return v->term_matrix<MatrixType::DGC>(corpus, nbuckets, true, ngram_min, ngram_max, term_weights);
  } else if (output == "row") {
    return v->term_matrix<MatrixType::DGR>(corpus, nbuckets, true, ngram_min, ngram_max, term_weights);
  } else {
    Rf_error("Invalid `output_type` (%s)", output.c_str());
  }
}

// [[Rcpp::export]]
SEXP C_tdm(SEXP corpus0, const DataFrame& vocabdf,
           const Nullable<NumericVector>& term_weights, const int nbuckets,
           std::string output,
           const int ngram_min, const int ngram_max) {
  Vocab* v = new Vocab(vocabdf);
  Corpus corpus(corpus0, v->separators());
  if (output == "triplet") {
    return v->term_matrix<MatrixType::DGT>(corpus, nbuckets, false, ngram_min, ngram_max, term_weights);
  } else if (output == "column") {
    return v->term_matrix<MatrixType::DGC>(corpus, nbuckets, false, ngram_min, ngram_max, term_weights);
  } else if (output == "row") {
    return v->term_matrix<MatrixType::DGR>(corpus, nbuckets, false, ngram_min, ngram_max, term_weights);
  } else {
    Rf_error("Invalid `output_type` (%s)", output.c_str());
  }
}

// [[Rcpp::export]]
SEXP C_tcm(SEXP corpus0, const DataFrame& vocabdf,
         const Nullable<NumericVector>& term_weights, const int nbuckets,
         const std::string& output,
         const size_t window_size, const std::vector<double>& window_weights,
         int ngram_min, int ngram_max, const std::string& context) {
  
  Vocab* v = new Vocab(vocabdf);
  Corpus corpus(corpus0, v->separators());

  ContextType context_type;
  if (context == "symmetric") {
    context_type = ContextType::SYMMETRIC;
  } else if (context == "right") {
    context_type = ContextType::RIGHT;
  } else if (context == "left") {
    context_type = ContextType::LEFT;
  } else {
    Rf_error("Invalid `context` (%s)", context.c_str());
  }
  
  if (output == "triplet") {
    return v->term_cooccurrence_matrix<MatrixType::DGT>(
      corpus, nbuckets, window_size, window_weights, ngram_min, ngram_max, context_type, term_weights);
  } else if (output == "column") {
    return v->term_cooccurrence_matrix<MatrixType::DGC>(
      corpus, nbuckets, window_size, window_weights, ngram_min, ngram_max, context_type, term_weights);
  } else if (output == "row") {
    return v->term_cooccurrence_matrix<MatrixType::DGR>(
      corpus, nbuckets, window_size, window_weights, ngram_min, ngram_max, context_type, term_weights);
  } else {
    Rf_error("Invalid `output_type` (%s)", output.c_str());
  }  
}

// [[Rcpp::export]]
LogicalVector C_is_ascii(const CharacterVector& vec) {
  LogicalVector out(vec.size());
  size_t N = vec.size();
  for (size_t i = 0; i < N; i++) {
    out[i] = is_ascii(vec[i].begin());
  }
  return out;
}

// [[Rcpp::export]]
CharacterVector C_wordgram(const CharacterVector& vec, int ngram_min, int ngram_max, std::string sep) {
  return wrap(wordgrams(as<vector<string>>(vec), ngram_min, ngram_max, sep));
}

// [[Rcpp::export]]
NumericVector C_ngram_weights(const NumericVector& weights, int ngram_min, int ngram_max) {
  return wrap(ngram_weights(as<vector<double>>(weights), ngram_min, ngram_max));
}


// [[Rcpp::export]]
SEXP C_tokenize(const CharacterVector& input, const std::string& seps) {
  R_len_t len = input.size();
  SEXP out = PROTECT(Rf_allocVector(VECSXP, len));
  const char* cseps = seps.c_str();
  for (R_len_t i = 0; i < len; i++) {
    const char* doc = input[i];
    SET_VECTOR_ELT(out, i, wrap(tokenize(doc, cseps, !is_ascii(doc))));
  }
  UNPROTECT(1);
  return out;
}
