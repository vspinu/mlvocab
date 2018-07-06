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


/// TOKENIZER

inline vector<string> tokenize_ascii(const char* input, const char* seps) {
  vector<string> out;
  
  const char* beg = input;
  const char* end = input;
  const char* it = input;
  
  while (*it) {
    const char* tseps = seps;
    while (*tseps) {
      if (*it == *tseps) {
        if (end > beg) {
          string substr(beg, end);
          out.push_back(substr);
        }
        beg = it + 1;
        break;
      } 
      tseps++;
    }
    it++;
    if (!(*tseps)) {
      // no match
      end = it;
    }
  }
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
  bool is_list;
  R_xlen_t len;
  string seps;

 public:

  Corpus(SEXP data) {
    Corpus(data, " \t\n\r");
  }
  
  Corpus(SEXP data, const string& seps) {
    switch (TYPEOF(data)){
     case VECSXP: is_list = true; break;
     case STRSXP: is_list = false; break;
     default: Rf_error("Corpus can be a list of strings or a character vector");
    }
    this->data = data;
    this->len = Rf_xlength(data);
    this->seps = seps;
  }

  inline R_xlen_t size () const {
    return len;
  }
 
  SEXP names() const {
    return Rf_getAttrib(data, R_NamesSymbol);
  } 

  /* Nullable<const CharacterVector&> names() const { */
  /*   SEXP names = Rf_getAttrib(data, R_NamesSymbol); */
  /*   if (names == R_NilValue) { */
  /*     return Nullable<const CharacterVector&>(R_NilValue); */
  /*   } else { */
  /*     const CharacterVector cvnames(names); */
  /*     return Nullable<const CharacterVector&>(cvnames); */
  /*   } */
  /* }  */
  
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
      return tokenize_ascii(CHAR(STRING_ELT(data, i)), seps.c_str());
    }      
  }

  
};

#endif /* VOCAB_CORPUS_H */
