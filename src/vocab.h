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
  uint_fast32_t ngram_min;
  uint_fast32_t ngram_max;
  uint_fast32_t ndocs;
  uint_fast32_t nuniqterms;
  string sep_ngram;
  bool chargram;

  sparse_hash_map<string, uint_fast32_t> vocabix;
  vector<VocabEntry> vocab;

 public:
  Vocab();
  Vocab(const DataFrame df) {
    const IntegerVector& ngram = df.attr("ngram");
    ngram_min = ngram[0];
    ngram_max = ngram[1];
    const IntegerVector& ndocs = df.attr("document_count");
    this->ndocs = ndocs[0];
    const LogicalVector& chargram = df.attr("chargram");
    this->chargram = chargram[0];
    const CharacterVector& sep_ngram = df.attr("sep_ngram");
    this->sep_ngram = as<string>(sep_ngram[0]);

    const CharacterVector& terms = df["term"];
    const IntegerVector& term_count = df["term_count"];
    const IntegerVector& doc_count = df["doc_count"];

    size_t N = df.nrows();

    for (size_t i = 0; i < N; i++) {
      insert_entry(as<string>(terms[i]), term_count[i], doc_count[i]);
    }
  };

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
    out.attr("sep_ngram") = CharacterVector::create(sep_ngram);
    out.attr("chargram") = LogicalVector::create(chargram);
    out.attr("class") = CharacterVector::create("mlvocab_vocab", "text2vec_vocabulary", "data.frame");
    
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
      vocab[vit->second].n =+ n;
      vocab[vit->second].ndocs =+ ndocs;
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
      ngram_vec = wordgrams(as<vector<string>>(doc), ngram_min, ngram_max, sep_ngram);
      insert_doc(ngram_vec);
    }
  }

  
  /// EMBEDDING
  NumericMatrix embed_vocab(NumericMatrix& embeddings, bool by_row, 
                            int unknown_buckets, int max_per_bucket) {
    // embedding as columns for efficiency
    size_t nembs = vocabix.size() + unknown_buckets;
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
    if (unknown_buckets > 0) {
      fill_mean_embeddings(out, vocabix.size(), embeddings, by_row, max_per_bucket);
    }

    if (by_row) {
      out = transpose(out);
      rownames(out) = vocab_names(unknown_buckets);
    } else {
      colnames(out) = vocab_names(unknown_buckets);
    }
    return out;
  }

  
  /// IX SEQUENCE GENERATORS
  
  ListOf<IntegerVector> corpus2ixseq(const ListOf<const CharacterVector>& corpus,
                                     bool keep_unknown, int unknown_buckets, bool doreverse) {
    size_t CN = corpus.size();
    vector<vector<int>> out;
    out.reserve(CN);
    for (size_t i = 0; i < CN; i++) {
      const CharacterVector& doc(corpus[i]);
      size_t ND = doc.size();
      vector<int> v;
      v.reserve(ND);
      for (auto el : doc) {
        push_ix_maybe(v, as<string>(el), keep_unknown, unknown_buckets);
      }
      if (doreverse)
        reverse(v.begin(), v.end());
      out.push_back(v);
    }
    return wrap(out);
  }
  
  IntegerMatrix corpus2ixmat(const ListOf<const CharacterVector>& corpus,
                             size_t maxlen, bool pad_right, bool trunc_right,
                             bool keep_unknown, int unknown_buckets, bool doreverse) {
    
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
          push_ix_maybe(v, as<string>(*it), keep_unknown, unknown_buckets);
        }
      } else {
        for (auto it = doc.end(); it-- != doc.begin() && v.size() < maxlen;) {
          push_ix_maybe(v, as<string>(*it), keep_unknown, unknown_buckets);
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
  SEXP term_matrix(const ListOf<CharacterVector>& corpus, int unknown_buckets, bool dtm = true) {
    TripletMatrix* tm = new TripletMatrix();
    int& doc_dim = dtm ? tm->nrow : tm->ncol; 

    size_t csize = corpus.size();
    shm_string_iter vit;
    
    for (size_t i = 0; i < csize; i++) {
      const CharacterVector doc(corpus[i]);
      for (auto termc : doc) {
        string term = as<string>(termc);
        vit = vocabix.find(term);
        // Matrix classes are 0-based indexed
        if (vit == vocabix.end()) {
          if (unknown_buckets > 0) {
            tm->add(doc_dim, murmur3hash(term) % unknown_buckets + vocab.size(), 1.0, dtm);
          } 
        } else {
          tm->add(doc_dim, vit->second, 1.0, dtm);
        }
      }
      doc_dim++;
    }

    if (dtm) {
      return tm->get(mattype, corpus.attr("names"), vocab_names(unknown_buckets));
    } else {
      return tm->get(mattype, vocab_names(unknown_buckets), corpus.attr("names"));
    }
  }
  
 private: // utils
  
  void push_ix_maybe(vector<int>& v, const string& term, bool keep_unknown, int unknown_buckets) {
    shm_string_iter vit = vocabix.find(term);
    if (vit == vocabix.end()) {
      if (keep_unknown) {
        if (unknown_buckets > 0) {
          v.push_back(murmur3hash(term) % unknown_buckets + vocab.size() + 1);
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
      bkt = murmur3hash(embnames[i].begin()) % n;
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

  CharacterVector vocab_names(size_t nbuckets) {
    CharacterVector out(vocab.size() + nbuckets);
    size_t i = 0;
    for (const VocabEntry& ve : vocab) {
      out[i] = Rf_mkChar(ve.term.c_str());
      i++;
    }
    string tmpname = "";
    for (size_t bkt = 1; bkt <= nbuckets; bkt++) {
      out[i] = "bkt" + to_string(bkt);
      i++;
    }
    return out;
  }

};


#endif // MAGISTRAL_VOCAB_H
