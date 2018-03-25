 
#include "vocab.h"
 
// [[Rcpp::export]]
DataFrame C_vocab(const ListOf<const CharacterVector>& corpus, const DataFrame& oldvocab) {
  Vocab* vocab = new Vocab(oldvocab);
  vocab->insert_corpus(corpus);
  return vocab->df();
}
 
// [[Rcpp::export]]
NumericMatrix C_embed_vocab(const DataFrame& vocabdf, NumericMatrix& embeddings, bool by_row,
                            int unknown_buckets, int min_to_average) {
  Vocab* v = new Vocab(vocabdf);
  return(v->embed_vocab(embeddings, by_row, unknown_buckets, min_to_average));
}

// [[Rcpp::export]]
List C_corpus2ixseq(const ListOf<const CharacterVector>& corpus, const DataFrame& vocabdf,
                    bool keep_unknown, int unknown_buckets, bool reverse) {
  Vocab* v = new Vocab(vocabdf);
  return(v->corpus2ixseq(corpus, keep_unknown, unknown_buckets, reverse));
}

// [[Rcpp::export]]
IntegerMatrix C_corpus2ixmat(const ListOf<const CharacterVector>& corpus, const DataFrame& vocabdf,
                             int maxlen, bool pad_right, bool trunc_right,
                             bool keep_unknown, int unknown_buckets, bool reverse) {
  Vocab* v = new Vocab(vocabdf);
  return(v->corpus2ixmat(corpus, maxlen, pad_right, trunc_right, keep_unknown, unknown_buckets, reverse));
}

// [[Rcpp::export]]
S4 C_dtm(const ListOf<CharacterVector>& corpus, const DataFrame& vocabdf,
         int unknown_buckets, std::string output) {
  Vocab* v = new Vocab(vocabdf);
  if (output == "triplet") {
    return v->term_matrix<MatrixType::DGT>(corpus, unknown_buckets, true);
  } else if (output == "column") {
    return v->term_matrix<MatrixType::DGC>(corpus, unknown_buckets, true);
  } else if (output == "row") {
    return v->term_matrix<MatrixType::DGR>(corpus, unknown_buckets, true);
  } else {
    Rf_error("Invalid `output_type` (%s)", output.c_str());
  }
}

// [[Rcpp::export]]
S4 C_tdm(const ListOf<CharacterVector>& corpus, const DataFrame& vocabdf,
         int unknown_buckets, std::string output) {
  Vocab* v = new Vocab(vocabdf);
  if (output == "triplet") {
    return v->term_matrix<MatrixType::DGT>(corpus, unknown_buckets, false);
  } else if (output == "column") {
    return v->term_matrix<MatrixType::DGC>(corpus, unknown_buckets, false);
  } else if (output == "row") {
    return v->term_matrix<MatrixType::DGR>(corpus, unknown_buckets, false);
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
