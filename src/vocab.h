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
typedef sparse_hash_map<string, uint32_t>::iterator sparse_hash_map_iter;



/// VOCAB CODE

class VocabEntry {
 public:
  VocabEntry(size_t id, size_t n = 1, size_t ndocs = 1):
    id(id), n(n), ndocs(ndocs) {};
  size_t id;
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
    for(auto it : vocabix) {
      // return UTF8: see https://github.com/dselivanov/text2vec/issues/101
      terms[i] = Rf_mkCharLenCE(it.first.c_str(), it.first.size(), CE_UTF8);
      term_counts[i] = vocab[it.second].n;
      doc_counts[i] = vocab[it.second].ndocs;
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
      vocab.push_back(VocabEntry(id, n, ndocs));
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
        vocab.push_back(VocabEntry(id, 1, 0));
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

  
  /// IX SEQUENCE GENERATORS
  
  ListOf<IntegerVector> corpus2ixseq(const ListOf<const CharacterVector>& corpus,
                                     bool keep_unknown, int unknown_buckets) {
    size_t CN = corpus.size();
    vector<vector<int>> out;
    out.reserve(CN);
    sparse_hash_map_iter vit;
    for (size_t i = 0; i < CN; i++) {
      const CharacterVector& doc(corpus[i]);
      size_t ND = doc.size();
      vector<int> v;
      v.reserve(ND);
      for (auto el : doc) {
        push_ix_maybe(v, as<string>(el), keep_unknown, unknown_buckets);
      }
      out.push_back(v);
    }
    return wrap(out);
  }
  
  IntegerMatrix corpus2ixmat(const ListOf<const CharacterVector>& corpus,
                             size_t maxlen, bool pad_right, bool trunc_right,
                             bool keep_unknown, int unknown_buckets) {
    
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
};


#endif // MAGISTRAL_VOCAB_H
