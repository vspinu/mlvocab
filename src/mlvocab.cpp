
#include "vocab.h"

// [[Rcpp::export]]
DataFrame C_vocab(SEXP corpus0, const DataFrame& oldvocab) {
  Vocab vocab(oldvocab);
  Corpus corpus(corpus0, vocab.separators());
  vocab.insert_corpus(corpus);
  // this takes a fraction of a second even for big vocabs
  vocab.sort();
  return vocab.df();
}

// [[Rcpp::export]]
NumericMatrix C_prune_embeddings(const DataFrame& vocabdf, NumericMatrix& embeddings, bool by_row,
                            int nbuckets, int min_to_average) {
  Vocab v(vocabdf);
  return(v.prune_embeddings(embeddings, by_row, nbuckets, min_to_average));
}

// [[Rcpp::export]]
DataFrame C_rehash_vocab(const DataFrame& pruned_vocabdf, const DataFrame& orig_vocabdf, const int nbuckets) {
  Vocab v(pruned_vocabdf);
  v.sort();
  v.rehash_unknowns(orig_vocabdf, nbuckets);
  return v.df();
}

// [[Rcpp::export]]
SEXP C_corpus2ixseq(SEXP corpus0, const DataFrame& vocabdf,
                    bool keep_unknown, int nbuckets, bool reverse) {
  Vocab v(vocabdf);
  Corpus corpus(corpus0, v.separators());
  return(v.corpus2ixseq(corpus, keep_unknown, nbuckets, reverse));
}

// [[Rcpp::export]]
DataFrame C_corpus2ixdf(SEXP corpus0, const DataFrame& vocabdf,
                        bool keep_unknown, int nbuckets,
                        bool reverse, bool asfactor) {
  Vocab v(vocabdf);
  Corpus corpus(corpus0, v.separators());
  return(v.corpus2ixdf(corpus, keep_unknown, nbuckets, reverse, asfactor));
}

// [[Rcpp::export]]
IntegerMatrix C_corpus2ixmat(SEXP corpus0, const DataFrame& vocabdf,
                             int maxlen, bool pad_right, bool trunc_right,
                             bool keep_unknown, int nbuckets, bool reverse) {
  Vocab v(vocabdf);
  const Corpus corpus(corpus0, v.separators());
  return v.corpus2ixmat(corpus, maxlen, pad_right, trunc_right, keep_unknown, nbuckets, reverse);
}

// [[Rcpp::export]]
SEXP C_dtm(SEXP corpus0, const DataFrame& vocabdf,
           const Nullable<NumericVector>& term_weights,
           const int nbuckets,
           const std::string& output,
           const int ngram_min,
           const int ngram_max) {
  Vocab v(vocabdf);
  Corpus corpus(corpus0, v.separators());
  if (output == "triplet") {
    return v.term_matrix<MatrixType::DGT>(corpus, nbuckets, true, ngram_min, ngram_max, term_weights);
  } else if (output == "column") {
    return v.term_matrix<MatrixType::DGC>(corpus, nbuckets, true, ngram_min, ngram_max, term_weights);
  } else if (output == "row") {
    return v.term_matrix<MatrixType::DGR>(corpus, nbuckets, true, ngram_min, ngram_max, term_weights);
  } else {
    Rf_error("Invalid `output_type` (%s)", output.c_str());
  }
}

// [[Rcpp::export]]
SEXP C_tdm(SEXP corpus0, const DataFrame& vocabdf,
           const Nullable<NumericVector>& term_weights,
           const int nbuckets,
           const std::string& output,
           const int ngram_min,
           const int ngram_max) {
  Vocab v(vocabdf);
  Corpus corpus(corpus0, v.separators());
  if (output == "triplet") {
    return v.term_matrix<MatrixType::DGT>(corpus, nbuckets, false, ngram_min, ngram_max, term_weights);
  } else if (output == "column") {
    return v.term_matrix<MatrixType::DGC>(corpus, nbuckets, false, ngram_min, ngram_max, term_weights);
  } else if (output == "row") {
    return v.term_matrix<MatrixType::DGR>(corpus, nbuckets, false, ngram_min, ngram_max, term_weights);
  } else {
    Rf_error("Invalid `output_type` (%s)", output.c_str());
  }
}

// [[Rcpp::export]]
SEXP C_tcm(SEXP corpus0, const DataFrame& vocabdf,
           const Nullable<NumericVector>& term_weights,
           const int nbuckets,
           const std::string& output,
           const size_t window_size,
           const std::vector<double>& window_weights,
           const std::string& context,
           const int ngram_min,
           const int ngram_max) {

  Vocab v(vocabdf);
  Corpus corpus(corpus0, v.separators());

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
    return v.term_cooccurrence_matrix<MatrixType::DGT>(
      corpus, nbuckets, window_size, window_weights, ngram_min, ngram_max, context_type, term_weights);
  } else if (output == "column") {
    return v.term_cooccurrence_matrix<MatrixType::DGC>(
      corpus, nbuckets, window_size, window_weights, ngram_min, ngram_max, context_type, term_weights);
  } else if (output == "row") {
    return v.term_cooccurrence_matrix<MatrixType::DGR>(
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
SEXP C_tokenize(SEXP input, SEXP rx) {
  R_len_t len = Rf_xlength(input);
  string srx = translate_separators(rx);
  regex rgx(srx,
            regex_constants::ECMAScript | regex_constants::nosubs |
            regex_constants::optimize);
  wregex wrgx(to_utf32(srx),
              regex_constants::ECMAScript | regex_constants::nosubs |
              regex_constants::optimize | regex_constants::collate);
  bool do_utf8 = !is_ascii(srx.c_str());
  SEXP out = PROTECT(Rf_allocVector(VECSXP, len));
  for (R_len_t i = 0; i < len; i++) {
    vector<string> el;
    SEXP in = STRING_ELT(input, i);
    if (in == NA_STRING) {
      SET_VECTOR_ELT(out, i, Rf_ScalarString(NA_STRING));
      continue;
    }
    const char* doc = CHAR(in);
    if (do_utf8) {
      if (is_ascii(doc))
        el = tokenize(doc, rgx);
      else {
        el = wtokenize(doc, wrgx);
      }
    } else {
      el = tokenize(doc, rgx);
    }
    SET_VECTOR_ELT(out, i, toRstrvec(el));
  }
  UNPROTECT(1);
  return out;
}


/* // does not sort unknowns at the end; for speed test only */
/* // [[Rcpp::export]] */
/* DataFrame C_sort_vocab(const DataFrame& vocabdf, bool sort = true) { */
/*   Vocab vocab(vocabdf); */
/*   if (sort) */
/*     vocab.sort(); */
/*   return vocab.df(); */
/* } */
