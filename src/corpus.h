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

 
#ifndef VOCAB_CORPUS_H
#define VOCAB_CORPUS_H

#include "Rinternals.h"
#include <cstring>
#include <string>
#include <vector>

using namespace std;

// utf-8 length computation (taken from R's util.c)
// See also https://stackoverflow.com/a/28311607/453735
// and https://github.com/wch/r-source/blob/trunk/src/main/valid_utf8.h#L67

/* Number of additional bytes */
static const unsigned char utf8_table4[] =
  {
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
   2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
   3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5
  };

inline char utf8clen(char c) {
  /* This allows through 8-bit chars 10xxxxxx, which are invalid */
  if ((c & 0xc0) != 0xc0) return 1;
  return 1 + utf8_table4[c & 0x3f];
}


/// TOKENIZER

inline string translate_separators(SEXP seps) {
  if (TYPEOF(seps) != STRSXP || Rf_xlength(seps) != 1) {
    Rf_error("`seps` must be a scalar string"); 
  }
  string out(Rf_translateCharUTF8(STRING_ELT(seps, 0)));
  return out;
}

inline vector<string> tokenize(const char* input, const char* seps, bool utf8 = false) {
  vector<string> out;
  
  const char* beg = input;
  const char* end = input;
  const char* it = input;
  char in_l = 1, sep_l = 1;
  
  while (*it) {
    if (utf8) {
      in_l = utf8clen(*it);
    }
    const char* tseps = seps;
    bool matched = false;
    while (*tseps) {
      if (utf8) {
        sep_l = utf8clen(*tseps);
        matched = in_l == sep_l && strncmp(tseps, it, sep_l) == 0;
      } else {
        matched = *it == *tseps;
      }
      if (matched) {
        if (end > beg) {
          string substr(beg, end);
          out.push_back(substr);
        }
        beg = it + in_l;
        break;
      }
      tseps += sep_l;
    }
    it += in_l;
    if (!matched) {
      // no match
      end = it;
    }
  }
  // last piece
  if (end > beg) {
    string substr(beg, end);
    out.push_back(substr);
  }
  return out;
}


/// CORPUS

// wrapper for transient usage only, because it relies on SEXP pointer which is
// assumed protected.
class Corpus
{
  SEXP data;
  SEXP names_;
  bool is_list;
  R_xlen_t len;
  string seps;
  bool do_utf8 = false;
  
 public:

  Corpus(SEXP data) {
    Corpus(data, " \t\n\r");
  }
  
  Corpus(SEXP data, const string& seps) {
    if (!is_ascii(seps.c_str()))
      this->do_utf8 = true;
    this->seps = seps;
    if (Rf_inherits(data, "data.frame")) {
      if (Rf_xlength(data) < 2)
        Rf_error("A data.frame `corpus` must have at least two columns");
      names_ = VECTOR_ELT(data, 0);
      data = VECTOR_ELT(data,1);
    } else {
      names_ = Rf_getAttrib(data, R_NamesSymbol);
    }
    switch (TYPEOF(data)) {
     case VECSXP: is_list = true; break;
     case STRSXP: is_list = false; break;
     default: Rf_error("Corpus must be a list of strings, a character vector or a two column data.frame");
    }
    this->data = data;
    this->len = Rf_xlength(data);
  }

  inline R_xlen_t size () const {
    return len;
  }
 
  SEXP names() const {
    return names_;
  } 
  
  const vector<string> operator[](R_xlen_t i) const {
    if (is_list) {
      SEXP doc = VECTOR_ELT(data, i);
      if (!Rf_isString(doc))
        Rf_error("Each element of a corpus list must be a character vector");
      R_xlen_t N = Rf_xlength(doc);
      vector<string> out;
      out.reserve(N);
      for (R_xlen_t i = 0; i < N; i++) {
        string token(CHAR(STRING_ELT(doc, i)));
        out.push_back(token);
      }
      return out;
    } else {
      // character vector
      if (do_utf8) {
        const char* doc = CHAR(STRING_ELT(data, i));
        if (is_ascii(doc))
          return tokenize(doc, seps.c_str(), false);
        else
          return tokenize(doc, seps.c_str(), true);
      } else {
        return tokenize(CHAR(STRING_ELT(data, i)), seps.c_str(), true);
      }
    }      
  }

  
};

#endif /* VOCAB_CORPUS_H */
