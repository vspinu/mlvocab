// Copyright (C) 2018  Vitalie Spinu
// This file is part of mlvocab
//
// mlvocab free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 2 of the License, or (at your option) any
// later version.
//
// mlvocab is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// more details.
//
// You should have received a copy of the GNU General Public License along with
// mlvocab. If not, see <http://www.gnu.org/licenses/>.


#ifndef MAGISTRAL_VOCAB_H
#define MAGISTRAL_VOCAB_H

#include "common.h"
#include "corpus.h"
#include "TripletMatrix.h"
#include "PriSecMatrix.h"
#include "hash.h"
#include "ngram.h"
#include <algorithm>
#include <unordered_set>
#include <omp.h>

class VocabEntry {
 public:
  VocabEntry(string term, size_t n = 1, size_t ndocs = 1):
    term(term), n(n), ndocs(ndocs) {};
  string term;
  size_t n;      // total count in corpus
  size_t ndocs;  // total count of docs

  bool operator < (const VocabEntry& other) const {
    return (n > other.n);
  }
};

class Vocab {

 private:

  int ngram_min;
  int ngram_max;
  string ngram_sep;
  string regex;
  int ndocs;
  int nbuckets;
  int nuniqterms;
  /* bool chargram; */
  hashmap<string, uint_fast32_t> vocabix;

  vector<VocabEntry> vocab;

 public:

  Vocab(const DataFrame& df) {

    const IntegerVector& ngram = df.attr("ngram");
    ngram_min = ngram[0];
    ngram_max = ngram[1];
    const IntegerVector& ndocs = df.attr("document_count");
    this->ndocs = ndocs[0];
    const IntegerVector& ubuckets = df.attr("nbuckets");
    this->nbuckets = ubuckets[0];
    const CharacterVector& ngram_sep = df.attr("ngram_sep");
    this->regex = translate_separators(df.attr("regex"));
    this->ngram_sep = as<string>(ngram_sep[0]);

    const CharacterVector& terms = df["term"];
    const IntegerVector& term_count = df["term_count"];
    const IntegerVector& doc_count = df["doc_count"];

    size_t N = df.nrow();

    // populate the vocabix
    for (size_t i = 0; i < N; i++) {
      const char* termc = terms[i];
      if (!is_bkt_name(termc)) {
        // Core terms only. Unknown buckets are generated on the fly when needed
        // and we don't allow updating of pruned vocabs.
        insert_entry(string(termc), term_count[i], doc_count[i]);
      }
    }

  };

  
  // Retrieve in DF form

  DataFrame df() {
    size_t N = vocab.size();
    CharacterVector terms(N);
    IntegerVector term_counts(N);
    IntegerVector doc_counts(N);

    size_t i = 0;
    for(const VocabEntry& ve : vocab) {
      // Output UTF8 which is not correct if input encoding is non-ascii or
      // non-utf8. But we explicitly support UTF-8 only.
      terms[i] = Rf_mkCharCE(ve.term.c_str(), CE_UTF8);
      term_counts[i] = ve.n;
      doc_counts[i] = ve.ndocs;
      i++;
    }

    DataFrame out = DataFrame::create(_["term"] = terms,
                                      _["term_count"] = term_counts,
                                      _["doc_count"] = doc_counts,
                                      _["stringsAsFactors"] = false);
    out.attr("ngram") = IntegerVector::create(ngram_min, ngram_max);
    out.attr("document_count") = IntegerVector::create(ndocs);
    out.attr("nbuckets") = IntegerVector::create(nbuckets);
    out.attr("ngram_sep") = CharacterVector::create(ngram_sep);
    if (regex != "")
      out.attr("regex") = CharacterVector::create(regex);
    out.attr("class") = CharacterVector::create("mlvocab_vocab", "data.frame");

    return out;
  }

  
  /// INTERNAL UTILITIES

  const char* separators() {
    return this->regex.c_str();
  }

  void sort() {
    vector<size_t> perm = sorting_permutation(vocab);
    apply_permutation_in_place(vocab, perm);
  }

  void rehash_unknowns(const DataFrame& orig_vocabdf, size_t nbuckets) {

    const IntegerVector& rubuckets = orig_vocabdf.attr("nbuckets");
    size_t ubuckets = rubuckets[0];

    if (ubuckets > 0 && ubuckets != nbuckets)
      Rf_error("Cannot rehash the vocab with a different number of buckets");

    if (nbuckets > 0) {

      const CharacterVector& terms = orig_vocabdf["term"];
      const IntegerVector& term_count = orig_vocabdf["term_count"];
      const IntegerVector& doc_count = orig_vocabdf["doc_count"];
      size_t N = orig_vocabdf.nrow();

      // 1) put all bkt names at the end of the vocab first
      for (size_t bkt = 1; bkt <= nbuckets; bkt++) {
        insert_entry(bkt_name(bkt), 0, 0);
      }

      // 2) add all unknowns at the end
      hashmap_string_iter vit;
      for (size_t i = 0; i < N; i++) {
        const char* termc = terms[i];
        string term(termc);
        if (is_bkt_name(termc)) {
          // 2.a) existing unknowns
          insert_entry(term, term_count[i], doc_count[i]);
        } else {
          // 2.b) new unknowns
          vit = vocabix.find(term);
          if (vit == vocabix.end()) {
            uint_fast32_t bkt = murmur3hash(term) % nbuckets + 1;
            insert_entry(bkt_name(bkt), term_count[i], doc_count[i]);
          }
        }
      }

      this->nbuckets = nbuckets;
    }
  }

  
  /// INSERT

  void insert_entry(const string& term, size_t n = 1, size_t ndocs = 1) {
    hashmap_string_iter vit = vocabix.find(term);
    if (vit == vocabix.end()) {
      size_t id = vocab.size();
      vocabix.insert(make_pair(term, id));
      vocab.push_back(VocabEntry(term, n, ndocs));
    } else {
      vocab[vit->second].n += n;
      vocab[vit->second].ndocs += ndocs;
    }
  }

  void insert_doc(const vector<string>& doc) {
    hashmap_string_iter vit;
    unordered_set<size_t> termset;
    size_t id;
    for (const string& term : doc) {
      vit = vocabix.find(term);
      if (vit == vocabix.end()) {
        id = vocab.size();
        vocabix.insert(make_pair(term, id));
        vocab.push_back(VocabEntry(term, 1, 0));
      } else {
        id = vit->second;
        vocab[id].n = vocab[id].n + 1;
      }
      termset.insert(id);
    }
    for (const size_t id : termset) {
      vocab[id].ndocs++;
    }
    ndocs++;
  }

  void insert_corpus(const Corpus& corpus) {
    vector<string> ngram_vec;
    R_xlen_t CN = corpus.size();
    for(R_xlen_t i = 0; i < CN; i++) {
      ngram_vec = wordgrams(corpus[i], ngram_min, ngram_max, ngram_sep);
      insert_doc(ngram_vec);
    }
  }

  
  /// EMBEDDING

  NumericMatrix prune_embeddings(NumericMatrix& embeddings, bool by_row,
                            int nbuckets, int max_per_bucket) {
    // embedding as columns for efficiency
    size_t nembs = vocabix.size() + nbuckets;
    size_t esize = by_row ? embeddings.ncol() : embeddings.nrow();
    size_t nallembs = by_row ? embeddings.nrow() : embeddings.ncol();

    CharacterVector embnames = by_row ? rownames(embeddings) : colnames(embeddings);
    hashmap<string, uint_fast32_t> embmap;
    uint_fast32_t i = 0;
    for (auto nm : embnames) {
      const string term = as<string>(nm);
      embmap.insert(make_pair(term, i));
      i++;
    }
    vector<size_t> missing_terms;
    missing_terms.reserve(vocab.size() / 10);
    NumericMatrix out(esize, nembs);

    hashmap_string_iter eit;
    string term;
    for (size_t i = 0; i < vocab.size(); i++) {
      const string& term = vocab[i].term;
      eit = embmap.find(term);
      if (eit == embmap.end()) {
        missing_terms.push_back(i);
      } else {
        // copy embedding into output matrix
        NumericMatrix::Column ocol = out(_, i);
        if (by_row) {
          NumericMatrix::Row erow = embeddings(eit->second, _);
          copy(erow.begin(), erow.end(), ocol.begin());
        } else {
          NumericMatrix::Column ecol = embeddings(_, eit->second);
          copy(ecol.begin(), ecol.end(), ocol.begin());
        }
      }
    }

    // fill missing terms with average embeddings
    if (missing_terms.size() > 0) {
      size_t nmissing = missing_terms.size();;
      size_t n = std::max(size_t{1}, std::min(nallembs/max_per_bucket, nmissing));
      NumericMatrix meanembs(esize, n);
      fill_mean_embeddings(meanembs, 0, embeddings, by_row, max_per_bucket);
      for (size_t i = 0; i < nmissing; i++) {
        /* size_t bkt = murmur3hash(vocab[i].term) % n; */
        size_t bkt = i % n;
        /* Rprintf("bkt:%d nembs:%d mpb:%d n:%d\n", bkt, nembs, max_per_bucket,n); */
        NumericMatrix::Column ecol = meanembs(_, bkt);
        copy(ecol.begin(), ecol.end(), out(_, missing_terms[i]).begin());
      }
    }

    // fill unknown buckets with average embeddings
    if (nbuckets > 0) {
      fill_mean_embeddings(out, vocabix.size(), embeddings, by_row, max_per_bucket);
    }

    if (by_row) {
      out = transpose(out);
      rownames(out) = vocab_names(nbuckets);
    } else {
      colnames(out) = vocab_names(nbuckets);
    }
    return out;
  }

  
  /// IX SEQUENCE GENERATORS

  inline vector<int> doc2ixseq(const vector<string> doc, bool keep_unknown, int nbuckets, bool doreverse) {
    size_t ND = doc.size();
    vector<int> v;
    v.reserve(ND);
    for (auto el : doc) {
      push_ix_maybe(v, el, keep_unknown, nbuckets);
    }
    if (doreverse)
      reverse(v.begin(), v.end());
    return v;
  }

  SEXP corpus2ixseq(const Corpus& corpus, bool keep_unknown, int nbuckets, bool doreverse) {
    size_t CN = corpus.size();
    SEXP out = PROTECT(Rf_allocVector(VECSXP, CN));
    for (size_t i = 0; i < CN; i++) {
      vector<int> docix = doc2ixseq(corpus[i], keep_unknown, nbuckets, doreverse);
      SEXP rdocix = Rf_allocVector(INTSXP, docix.size());
      std::copy(docix.begin(), docix.end(), INTEGER(rdocix));
      SET_VECTOR_ELT(out, i, rdocix);
    }
    Rf_setAttrib(out, R_NamesSymbol, corpus.names());
    UNPROTECT(1);
    return out;
  }

  DataFrame corpus2ixdf(const Corpus& corpus, bool keep_unknown, int nbuckets,
                        bool doreverse, bool asfactor) {
    R_xlen_t CN = corpus.size();
    vector<int> ixes;
    ixes.reserve(CN*3);
    vector<int> reps(CN);
    for (R_xlen_t i = 0; i < CN; i++) {
      vector<int> ix = doc2ixseq(corpus[i], keep_unknown, nbuckets, doreverse);
      ixes.insert(ixes.end(), ix.begin(), ix.end());
      reps[i] = ix.size();
    }
    bool no_id = corpus.id() == R_NilValue;
    SEXP outdf;
    if (no_id) {
      // use integer ids if no names
      SEXP obj = PROTECT(Rf_allocVector(INTSXP, CN));
      int* pt = INTEGER(obj);
      for (R_xlen_t i = 0; i < CN; i++)
        pt[i] = i+1;
      outdf = PROTECT(replicate_df(obj, reps, ixes.size()));
      UNPROTECT(1); // obj
    } else {
      outdf = PROTECT(replicate_df(corpus.id(), reps, ixes.size()));
    }
    SEXP ix  = wrap(ixes);
    SET_VECTOR_ELT(outdf, Rf_xlength(outdf) - 1, ix);
    if (asfactor) {
      Rf_setAttrib(ix, R_LevelsSymbol, vocab_names(nbuckets));
      Rf_setAttrib(ix, R_ClassSymbol, Rf_mkString("factor"));
    }
    UNPROTECT(1); // outdf
    return outdf;
  }

  IntegerMatrix corpus2ixmat(const Corpus& corpus,
                             size_t maxlen, bool pad_right, bool trunc_right,
                             bool keep_unknown, int nbuckets, bool doreverse) {

    size_t CN = corpus.size();
    IntegerMatrix out(CN, maxlen);
    vector<int> v;

    for (size_t i = 0; i < CN; i++) {
      v.clear();
      v.reserve(maxlen + 1);

      const vector<string> doc = corpus[i];

      if (trunc_right) {
        for (auto it = doc.begin(); it != doc.end() && v.size() < maxlen; ++it) {
          push_ix_maybe(v, *it, keep_unknown, nbuckets);
        }
      } else {
        for (auto it = doc.end(); it-- != doc.begin() && v.size() < maxlen;) {
          push_ix_maybe(v, *it, keep_unknown, nbuckets);
        }
      }

      if (doreverse)
        reverse(v.begin(), v.end());

      IntegerMatrix::Row row = out(i, _);
      if (pad_right) {
        if (trunc_right)
          copy(v.begin(), v.end(), row.begin());
        else
          copy(v.rbegin(), v.rend(), row.begin());
      } else {
        if (trunc_right)
          copy_backward(v.begin(), v.end(), row.end());
        else
          copy_backward(v.rbegin(), v.rend(), row.end());
      }
    }
    rownames(out) = corpus.names();
    return out;
  }

  
  /// DTM/TCM

  template <MatrixType mattype>
  SEXP term_matrix(const Corpus& corpus,
                   const int nbuckets,
                   const bool dtm,
                   const int ngram_min,
                   const int ngram_max,
                   const Nullable<NumericVector>& term_weights) {

    check_ngram_limits(ngram_min, ngram_max);

    size_t CN = corpus.size();
    PriSecMatrix mat(CN);

    /* omp_set_num_threads(8); */

#pragma omp parallel for
    for (size_t i = 0; i < CN; i++) {
      hashmap_string_iter vit;
      const vector<string> doc = wordgrams(corpus[i], ngram_min, ngram_max, ngram_sep);
      mat.alloc(i, doc.size());
      for (const string& term : doc) {
        vit = vocabix.find(term);
        // Matrix classes are 0-based indexed
        if (vit == vocabix.end()) {
          if (nbuckets > 0) {
            mat.set(i, murmur3hash(term) % nbuckets + vocab.size(), 1.0);
          }
        } else {
          mat.set(i, vit->second, 1.0);
        }
      }
      mat.sort_and_compact(i);
    }

    // term_weights is currently not used
    if (term_weights.isNotNull())
      mat.apply_weight(term_weights.get(), MatrixDimType::SECONDARY);

    SEXP out;
    if (dtm) {
      if (mattype == MatrixType::DGC) {
        mat.transpose();
        out = mat.get(mattype, vocab_names(nbuckets), corpus.names(), false, false);
      } else {
        out = mat.get(mattype, corpus.names(), vocab_names(nbuckets), false, true);
      }
    } else {
      if (mattype == MatrixType::DGR) {
        mat.transpose();
        out = mat.get(mattype, vocab_names(nbuckets), corpus.names(), false, true);
      } else {
        out = mat.get(mattype, corpus.names(), vocab_names(nbuckets), false, false);
      }
    }
    Rf_setAttrib(out, Rf_mkString("mlvocab_dtm"), Rf_ScalarLogical(dtm));
    return out;
  }

  template <MatrixType mattype>
  SEXP term_cooccurrence_matrix(const Corpus& corpus,
                                const int nbuckets,
                                const size_t window_size,
                                const vector<double>& window_weights,
                                const int ngram_min,
                                const int ngram_max,
                                const ContextType context,
                                const Nullable<NumericVector>& term_weights) {

    check_ngram_limits(ngram_min, ngram_max);

    size_t CN = corpus.size();
    size_t TN = vocab.size() + nbuckets;
    TripletMatrix tm(TN, TN);
    hashmap_string_iter vit;
    string term;
    vector<double> weights = ngram_weights(window_weights, ngram_min, ngram_max);
    size_t wsize = weights.size();

    for (size_t d = 0; d < CN; d++) {

      const vector<string> doc = wordgrams(corpus[d], ngram_min, ngram_max, ngram_sep);
      size_t dsize = doc.size();
      uint32_t iix, jix;
      for (size_t i = 0; i < dsize; i++) {

        // (i)-term outer loop
        term = doc[i];
        vit = vocabix.find(term);
        if (vit == vocabix.end()) {
          if (nbuckets > 0)
            iix = murmur3hash(term) % nbuckets + vocab.size();
          else
            continue;
        } else {
          iix = vit->second;
        }

        // (j)-term inner loop
        for (size_t w = 1; w < wsize && i + w < dsize; w++) {
          term = doc[i + w];
          vit = vocabix.find(term);
          if (vit == vocabix.end()) {
            if (nbuckets > 0)
              jix = murmur3hash(term) % nbuckets + vocab.size();
            else
              continue;
          } else {
            jix = vit->second;
          }

          // add to tcm
          switch(context) {
           case ContextType::SYMMETRIC:
             // populating upper triangle
             if (iix < jix) tm.add(iix, jix, weights[w]);
             else tm.add(jix, iix, weights[w]);
             break;
           case ContextType::RIGHT:
             tm.add(iix, jix, weights[w]); break;
           case ContextType::LEFT:
             tm.add(jix, iix, weights[w]); break;
          }
        }
      }
    }

    if (term_weights.isNotNull()) {
      tm.apply_weight(term_weights.get(), MatrixDimType::BOTH);
    }
    const CharacterVector& names = vocab_names(nbuckets);
    return tm.get(mattype, names, names, context == ContextType::SYMMETRIC);
  }

  CharacterVector vocab_names(uint32_t nbuckets) {
    CharacterVector out(vocab.size() + nbuckets);
    size_t i = 0;
    for (const VocabEntry& ve : vocab) {
      out[i] = Rf_mkChar(ve.term.c_str());
      i++;
    }
    string tmpname = "";
    for (uint32_t bkt = 1; bkt <= nbuckets; bkt++) {
      out[i] = bkt_name(bkt);
      i++;
    }
    return out;
  }

 private: // utils

  void set_df_attrs(SEXP df, const CharacterVector& names) {
    // NB: assumes non-empty df
    Rf_setAttrib(df, R_ClassSymbol, Rf_mkString("data.frame"));
    Rf_setAttrib(df, R_NamesSymbol, names);
    SEXP rnames = IntegerVector::create(NA_INTEGER, Rf_xlength(VECTOR_ELT(df, 0)));
    Rf_setAttrib(df, R_RowNamesSymbol, rnames);
  }

  
  // Could have been done at R level but it's obnoxiously slow and greedy.

  SEXP replicate_df(SEXP obj, const vector<int>& reps, R_xlen_t N) {
    // allocate full output DF here but don't initialize the last column
    if (Rf_inherits(obj, "data.frame")) {
      // obj is the full input df
      R_xlen_t len = Rf_xlength(obj);
      SEXP df = PROTECT(Rf_allocVector(VECSXP, len));
      for (R_xlen_t i = 0; i < len - 1; i++) {
        SET_VECTOR_ELT(df, i, replicate_sexp(VECTOR_ELT(obj, i), reps, N));
      }
      SEXP new_names = PROTECT(Rf_duplicate(Rf_getAttrib(obj, R_NamesSymbol)));
      R_xlen_t last_i = Rf_xlength(new_names) - 1;
      string ix_name = std::string(CHAR(STRING_ELT(new_names, last_i))) + "_term";
      SET_STRING_ELT(new_names, last_i, Rf_mkCharCE(ix_name.c_str(), CE_UTF8));
      set_df_attrs(df, new_names);
      // for data.table and tible sake
      Rf_setAttrib(df, R_ClassSymbol, Rf_getAttrib(obj, R_ClassSymbol));
      UNPROTECT(2);
      return df;
    } else {
      // obj is a primitive vector
      SEXP df = PROTECT(Rf_allocVector(VECSXP, 2));
      SET_VECTOR_ELT(df, 0, replicate_sexp(obj, reps, N));
      set_df_attrs(df, CharacterVector::create("id", "term"));
      UNPROTECT(1);
      return df;
    }
  }

  SEXP replicate_sexp(SEXP obj, const vector<int>& reps, R_xlen_t N) {
    SEXP id = PROTECT(Rf_allocVector(TYPEOF(obj), N));
    R_xlen_t CN = reps.size();
    R_xlen_t oi = 0;
    switch(TYPEOF(obj)) {
     case STRSXP:
       for (R_xlen_t i = 0; i < CN; i++) {
         int r = reps[i];
         SEXP nm = STRING_ELT(obj, i);
         for (int rr = 0; rr < r; rr++) {
           SET_STRING_ELT(id, oi, nm);
           oi++;
         }
       };
       break;
     case LGLSXP:
     case INTSXP: {
       int* idpx = INTEGER(id);
       int* nmpx = INTEGER(obj);
       for (R_xlen_t i = 0; i < CN; i++) {
         int r = reps[i];
         int nm = nmpx[i];
         for (int rr = 0; rr < r; rr++) {
           idpx[oi] = nm;
           oi++;
         }
       };
       if (Rf_inherits(obj, "factor")) {
         Rf_setAttrib(id, R_LevelsSymbol, Rf_getAttrib(obj, R_LevelsSymbol));
       }
     }; break;
     case REALSXP: {
       double* idpx = REAL(id);
       double* nmpx = REAL(obj);
       for (R_xlen_t i = 0; i < CN; i++) {
         int r = reps[i];
         double nm = nmpx[i];
         for (int rr = 0; rr < r; rr++) {
           idpx[oi] = nm;
           oi++;
         }
       };
     }; break;
     default: Rf_error("Type '%s' is not supported for document ids", Rf_type2char(TYPEOF(obj)));
    }
    Rf_setAttrib(id, R_ClassSymbol, Rf_getAttrib(obj, R_ClassSymbol));
    UNPROTECT(1);
    return id;
  }

  void push_ix_maybe(vector<int>& v, const string& term, bool keep_unknown, int nbuckets) {
    hashmap_string_iter vit = vocabix.find(term);
    // this one is used for ixs for which 0 is special; thus +1
    int uoffset = static_cast<int>(vocab.size()) + 1;
    if (vit == vocabix.end()) {
      if (keep_unknown) {
        if (nbuckets > 0) {
          v.push_back((nbuckets == 1 ? 0 : murmur3hash(term) % nbuckets) + uoffset);
        } else {
          v.push_back(0);
        }
      }
    } else {
      v.push_back(vit->second + 1);
    }
  }

  void fill_mean_embeddings(NumericMatrix& meanembs, size_t offset,
                            NumericMatrix& embeddings, bool by_row,
                            int max_per_bucket) {
    size_t n = meanembs.ncol() - offset;
    size_t nfull = n;
    size_t nallembs = by_row ? embeddings.nrow() : embeddings.ncol();
    CharacterVector embnames = by_row ? rownames(embeddings) : colnames(embeddings);
    vector<int> sofar(n);
    size_t bkt;
    /* Rprintf("nallembs:%d n:%d\n", nallembs, n); */
    for (size_t i = 0; i < nallembs; i++) {
      if (nfull == 0) break;
      if (n == 1) bkt = 0;
      else bkt = murmur3hash(embnames[i].begin()) % n;
      if (sofar[bkt] < max_per_bucket) {
        NumericMatrix::Column col = meanembs(_, offset + bkt);
        if (by_row) {
          col = col + embeddings(i, _);
        } else {
          /* Rprintf("col:%d bkt:%d i:%d sofar[bkt]:%d\n", offset + bkt, bkt, i, sofar[bkt]); */
          col = col + embeddings(_, i);
        }
        sofar[bkt]++;
        if (sofar[bkt] == max_per_bucket) nfull--;
      }
    }
    for (size_t bkt = 0; bkt < n; bkt++) {
      if (sofar[bkt] > 0) {
        NumericMatrix::Column col = meanembs(_, offset + bkt);
        col = col/static_cast<double>(sofar[bkt]);
      }
    }
  }

 inline string bkt_name(uint32_t bkt) {
    return "__" + to_string(bkt);
  }

  inline bool is_bkt_name(const char* termc) {
    return termc[0] == '_' && termc[1] == '_';
  }

  void check_ngram_limits(const int ngram_min, const int ngram_max) {
    if (ngram_min < this->ngram_min) {
      Rf_error("`ngram_min` (%d) is smaller than the vocabulary's `ngram_min` (%d)", ngram_min, this->ngram_min);
    }
    if (ngram_max > this->ngram_max) {
      Rf_error("`ngram_max` (%d) is larger than the vocabulary's `ngram_max` (%d)", ngram_max, this->ngram_max);
    }
  }

};


#endif // MAGISTRAL_VOCAB_H
