 
#include "vocab.h"
 
// [[Rcpp::export]]
DataFrame C_vocab(const ListOf<const CharacterVector>& corpus, const DataFrame& oldvocab) {
  Vocab* vocab = new Vocab(oldvocab);
  vocab->insert_corpus(corpus);
  return vocab->df();
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
NumericMatrix C_embed_vocab(const DataFrame& vocabdf, NumericMatrix& embeddings, bool by_row,
                            int unknown_buckets, int min_to_average) {
  Vocab* v = new Vocab(vocabdf);
  return(v->embed_vocab(embeddings, by_row, unknown_buckets, min_to_average));
}
