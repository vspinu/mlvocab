// Copyright (C) 2018  Vitalie Spinu
// This file is part of magistral_string
//
// magistral_string free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 2 of the License, or (at your option) any
// later version.
//
// magistral_string is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// more details.
//
// You should have received a copy of the GNU General Public License along with
// magistral_string.  If not, see <http://www.gnu.org/licenses/>.

 
#ifndef MAGISTRAL_VOCAB_H
#define MAGISTRAL_VOCAB_H

#include "common.h"
#include "hash.h"
#include "ngram.h"
#include <algorithm>
#include <unordered_set>


/// SPARSE HASH MAP

/// comment from text2vec:
// spp has calls to 'exit' on failure, which upsets R CMD check.
// We won't bump into them during normal test execution so just override
// it in the spp namespace before we include 'sparsepp'.
// https://github.com/hadley/testthat/blob/c7e8330867645c174f9a286d00eb0036cea78b0c/inst/include/testthat/testthat.h#L44-L50
// https://stackoverflow.com/questions/43263880/no-ambiguous-reference-error-even-after-using-namespace-directive/43294812
namespace spp {
inline void exit(int status) throw() {}
}

#include <sparsepp/spp.h>
using spp::sparse_hash_map;
using spp::sparse_hash_set;
typedef sparse_hash_map<string, uint_fast32_t>::iterator sparse_hash_map_iter;
typedef sparse_hash_set<string>::iterator sparse_hash_set_iter;



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
      // return UTF8: see https://github.com/dselivanov/text2vec/issues/101
      terms[i] = Rf_mkCharLenCE(ve.term.c_str(), ve.term.size(), CE_UTF8);
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
    sparse_hash_map_iter vit = vocabix.find(term);
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
    sparse_hash_map_iter vit;
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

    sparse_hash_map_iter eit;  
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
      return transpose(out);
    } else {
      return out;
    }
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

 private: // utils
  
  void push_ix_maybe(vector<int>& v, const string& term, bool keep_unknown, int unknown_buckets) {
    sparse_hash_map_iter vit = vocabix.find(term);
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
};


#endif // MAGISTRAL_VOCAB_H
