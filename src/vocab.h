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
#include "TripletMatrix.h"
#include "hash.h"
#include "ngram.h"
#include <algorithm>
#include <unordered_set>


/// VOCAB CODE

class VocabEntry {
 public:
  VocabEntry(string term, size_t n = 1, size_t ndocs = 1):
    term(term), n(n), ndocs(ndocs) {};
  string term;
  size_t n;      // total count in corpus
  size_t ndocs;  // total count of docs
};

class Vocab {
  
 private:

  int ngram_min;
  int ngram_max;
  string ngram_sep;
  int ndocs;
  int nbuckets;
  int nuniqterms;
  /* bool chargram; */

  sparse_hash_map<string, uint_fast32_t> vocabix;
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
    /* const LogicalVector& chargram = df.attr("chargram"); */
    /* this->chargram = chargram[0]; */
    const CharacterVector& ngram_sep = df.attr("ngram_sep");
    this->ngram_sep = as<string>(ngram_sep[0]);

    const CharacterVector& terms = df["term"];
    const IntegerVector& term_count = df["term_count"];
    const IntegerVector& doc_count = df["doc_count"];

    size_t N = df.nrow();

    for (size_t i = 0; i < N; i++) {
      const char* termc = terms[i];
      if (!is_bkt_name(termc)) {
        // core terms only 
        insert_entry(string(termc), term_count[i], doc_count[i]);
      }
    }

  };
 
  void rebucket_unknowns(const DataFrame& df, size_t nbuckets) {
    const IntegerVector& rubuckets = df.attr("nbuckets");
    size_t ubuckets = rubuckets[0];

    if (ubuckets > 0 && ubuckets != nbuckets)
      Rf_error("Trying to rehash a vocab with a different number of buckets");
    
    if (nbuckets > 0) {

      const CharacterVector& terms = df["term"];
      const IntegerVector& term_count = df["term_count"];
      const IntegerVector& doc_count = df["doc_count"];
      size_t N = df.nrow();

      for (size_t bkt = 1; bkt <= nbuckets; bkt++) {
        insert_entry(bkt_name(bkt), 0, 0);
      }
      
      // 2) rehash all "new" unknowns
      shm_string_iter vit;
      for (size_t i = 0; i < N; i++) {
        const char* termc = terms[i];
        string term(termc);
        if (is_bkt_name(termc)) {
          // insert unknown counts
          insert_entry(term, term_count[i], doc_count[i]);
        } else {
          // rehash new unknowns
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

  DataFrame df() {
    size_t N = vocab.size();
    CharacterVector terms(N);
    IntegerVector term_counts(N);
    IntegerVector doc_counts(N);

    size_t i = 0;
    for(const VocabEntry& ve : vocab) {
      // output UTF8 which is not correct if input encoding is non-ascii or
      // non-utf8. Consider saving encoding (and maybe strlen).
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
    out.attr("class") = CharacterVector::create("mlvocab_vocab", "data.frame");
    
    return out;
  }

  
  /// INSERT
  
  void insert_entry(const string& term, size_t n = 1, size_t ndocs = 1) {
    shm_string_iter vit = vocabix.find(term);
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
    shm_string_iter vit;
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

  void insert_corpus(const ListOf<const CharacterVector>& corpus) {
    vector<string> vec;
    vector<string> ngram_vec;
    for(const CharacterVector& doc : corpus) {
      ngram_vec = wordgrams(as<vector<string>>(doc), ngram_min, ngram_max, ngram_sep);
      insert_doc(ngram_vec);
    }
  }
  
  
  /// EMBEDDING
  
  NumericMatrix embed_vocab(NumericMatrix& embeddings, bool by_row, 
                            int nbuckets, int max_per_bucket) {
    // embedding as columns for efficiency
    size_t nembs = vocabix.size() + nbuckets;
    size_t esize = by_row ? embeddings.ncol() : embeddings.nrow();
    size_t nallembs = by_row ? embeddings.nrow() : embeddings.ncol();

    CharacterVector embnames = by_row ? rownames(embeddings) : colnames(embeddings);
    sparse_hash_map<string, uint_fast32_t> embmap;
    uint_fast32_t i = 0;
    for (auto nm : embnames) {
      const string term = as<string>(nm);
      embmap.insert(make_pair(term, i));
      i++;
    }
    vector<size_t> missing_terms;
    missing_terms.reserve(vocab.size() / 10);
    NumericMatrix out(esize, nembs);

    shm_string_iter eit;  
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
  
  ListOf<IntegerVector> corpus2ixseq(const ListOf<const CharacterVector>& corpus,
                                     bool keep_unknown, int nbuckets, bool doreverse) {
    size_t CN = corpus.size();
    vector<vector<int>> out;
    out.reserve(CN);
    for (size_t i = 0; i < CN; i++) {
      const CharacterVector& doc(corpus[i]);
      size_t ND = doc.size();
      vector<int> v;
      v.reserve(ND);
      for (auto el : doc) {
        push_ix_maybe(v, as<string>(el), keep_unknown, nbuckets);
      }
      if (doreverse)
        reverse(v.begin(), v.end());
      out.push_back(v);
    }
    return wrap(out);
  }
  
  IntegerMatrix corpus2ixmat(const ListOf<const CharacterVector>& corpus,
                             size_t maxlen, bool pad_right, bool trunc_right,
                             bool keep_unknown, int nbuckets, bool doreverse) {
    
    size_t CN = corpus.size();
    IntegerMatrix out(CN, maxlen);
    vector<int>v;
    CharacterVector::iterator beg, end;
    
    for (size_t i = 0; i < CN; i++) {
      v.clear();
      v.reserve(maxlen + 1);
      const CharacterVector& doc(corpus[i]);

      if (trunc_right) {
        for (auto it = doc.begin(); it != doc.end() && v.size() < maxlen; ++it) {
          push_ix_maybe(v, as<string>(*it), keep_unknown, nbuckets);
        }
      } else {
        for (auto it = doc.end(); it-- != doc.begin() && v.size() < maxlen;) {
          push_ix_maybe(v, as<string>(*it), keep_unknown, nbuckets);
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
    
    return out;
  }

  
  /// DTM/TCM

  template <MatrixType mattype>
  SEXP term_matrix(const ListOf<CharacterVector>& corpus, int nbuckets, bool dtm,
                   const int ngram_min, const int ngram_max,
                   const Nullable<NumericVector>& term_weights) {

    check_ngram_limits(ngram_min, ngram_max);

    TripletMatrix* tm = new TripletMatrix();
    int& doc_dim = dtm ? tm->nrow : tm->ncol; 

    size_t csize = corpus.size();
    shm_string_iter vit;
    
    for (size_t i = 0; i < csize; i++) {
      const vector<string> doc = wordgrams(as<vector<string>>(corpus[i]), ngram_min, ngram_max, ngram_sep);
      for (const string& term : doc) {
        vit = vocabix.find(term);
        // Matrix classes are 0-based indexed
        if (vit == vocabix.end()) {
          Rprintf("not found: %s\n", term.c_str());
          if (nbuckets > 0) {
            tm->add(doc_dim, murmur3hash(term) % nbuckets + vocab.size(), 1.0, dtm);
          } 
        } else {
          tm->add(doc_dim, vit->second, 1.0, dtm);
        }
      }
      doc_dim++;
    }

    if (dtm) {
      if (term_weights.isNotNull())
        tm->apply_weight(term_weights.get(), MatrixDimType::COL);
      SEXP out = tm->get(mattype, corpus.attr("names"), vocab_names(nbuckets));
      Rf_setAttrib(out, Rf_mkString("mlvocab_dtm"), Rf_ScalarLogical(1));
      return out;
    } else {
      if (term_weights.isNotNull())
        tm->apply_weight(term_weights.get(), MatrixDimType::ROW);
      SEXP out = tm->get(mattype, vocab_names(nbuckets), corpus.attr("names"));
      Rf_setAttrib(out, Rf_mkString("mlvocab_dtm"), Rf_ScalarLogical(0));
      return out;
    }
  }

  template <MatrixType mattype>
  SEXP term_cooccurrence_matrix(const ListOf<CharacterVector>& corpus, const int nbuckets,
                                const size_t window_size, const vector<double>& window_weights,
                                const int ngram_min, const int ngram_max,
                                const ContextType context,
                                const Nullable<NumericVector>& term_weights) {

    check_ngram_limits(ngram_min, ngram_max);
    
    TripletMatrix* tm = new TripletMatrix();
    size_t csize = corpus.size();
    shm_string_iter vit;
    string term;
    vector<double> weights = ngram_weights(window_weights, ngram_min, ngram_max);
    size_t wsize = weights.size();
    
    for (size_t d = 0; d < csize; d++) {
      
      const vector<string> doc = wordgrams(as<vector<string>>(corpus[d]), ngram_min, ngram_max, ngram_sep);
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
             if (iix < jix) tm->add(iix, jix, weights[w]); 
             else tm->add(jix, iix, weights[w]); 
             break;
           case ContextType::RIGHT:
             tm->add(iix, jix, weights[w]); break;
           case ContextType::LEFT:
             tm->add(jix, iix, weights[w]); break;
          }
        }
      }
    }

    if (term_weights.isNotNull()) {
      tm->apply_weight(term_weights.get(), MatrixDimType::BOTH);
    }
    const CharacterVector& names = vocab_names(nbuckets);
    return tm->get(mattype, names, names, context == ContextType::SYMMETRIC);
    
  }

 private: // utils
  
  void push_ix_maybe(vector<int>& v, const string& term, bool keep_unknown, int nbuckets) {
    shm_string_iter vit = vocabix.find(term);
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
