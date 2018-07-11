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
#include <codecvt>
#include <regex>

using namespace std;



/// TOKENIZER

inline string translate_separators(SEXP seps) {
  if (seps == R_NilValue)
    return "";
  if (TYPEOF(seps) != STRSXP || Rf_xlength(seps) != 1) {
    Rf_error("`seps` must be a scalar string");
  }
  string out(Rf_translateCharUTF8(STRING_ELT(seps, 0)));
  return out;
}

// from https://stackoverflow.com/a/43302460
inline std::string to_utf8(std::wstring const& utf32) {
  std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> cnv;
  std::string utf8 = cnv.to_bytes(utf32);
  if(cnv.converted() < utf32.size())
    Rf_error("Incomplete conversion");
  return utf8;
}

inline std::wstring to_utf32(std::string const& utf8) {
  std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> cnv;
  std::wstring utf32 = cnv.from_bytes(utf8);
  if(cnv.converted() < utf8.size())
    Rf_error("Incomplete conversion");
  return utf32;
}

inline vector<string> wtokenize(const char* doc, const wregex& wrgx) {
  vector<string> out;
  wstring udoc(to_utf32(doc));
  wsregex_token_iterator iter(udoc.begin(), udoc.end(), wrgx, -1);
  wsregex_token_iterator end;
  while (iter != end) {
    const wssub_match match = *iter;
    if (match.first < match.second) {
      out.push_back(to_utf8(wstring(match.first, match.second)));
    }
    iter++;
  }
  return out;
}

inline vector<string> tokenize(const char* doc, const regex& rgx) {
  vector<string> out;
  cregex_token_iterator iter(doc, doc + strlen(doc), rgx, -1);
  cregex_token_iterator end;
  while (iter != end) {
    csub_match match = *iter;
    if (match.first < match.second) {
      out.push_back(string(match.first, match.second));
    }
    iter++;
  }
  return out;
}


/// CORPUS

// Wrapper for transient usage only. We assume the SEXP data pointer is protected.
class Corpus
{
  SEXP data;
  SEXP corpus;
  SEXP nms;
  R_xlen_t len;
  string seps;
  regex rgx;
  wregex wrgx;
  bool is_list;
  bool do_utf8 = false;
  bool do_tokenize = false;

 public:

  Corpus(SEXP data) {
    Corpus(data, " \t\n\r");
  }

  Corpus(SEXP data, const string& seps) {
    if (!is_ascii(seps.c_str()))
      this->do_utf8 = true;
    if (seps != "") {
      this->do_tokenize = true;
    }
    this->seps = seps;
    this->data = data;
    if (Rf_inherits(data, "data.frame")) {
      if (Rf_xlength(data) < 2)
        Rf_error("A data.frame `corpus` must have at least two columns");
      if (Rf_isString(VECTOR_ELT(data, 0)))
        // TOTHINK: automatic conversion to characters seems like a bad idea
        this->nms = VECTOR_ELT(data, 0);
      else
        this->nms = R_NilValue;
      this->corpus = VECTOR_ELT(data, Rf_length(data) - 1); // last column
    } else {
      this->nms = Rf_getAttrib(data, R_NamesSymbol);
      this->corpus = data;
    }
    switch (TYPEOF(this->corpus)) {
     case VECSXP: this->is_list = true; break;
     case STRSXP: this->is_list = false; break;
     default: Rf_error("Corpus must be a list of strings, a character vector or a data.frame");
    }
    this->len = Rf_xlength(corpus);
    this->rgx = regex(seps,
                      regex_constants::ECMAScript | regex_constants::nosubs |
                      regex_constants::optimize);
    this->wrgx = wregex(to_utf32(seps),
                        regex_constants::ECMAScript | regex_constants::nosubs |
                        regex_constants::optimize | regex_constants::collate);
  }

  inline R_xlen_t size () const {
    return len;
  }

  SEXP names() const {
    return nms;
  }

  SEXP id() const {
    if (Rf_inherits(data, "data.frame"))
      // returning full data for simplicity (only needed in corpus2ixdf)
      return data;
    else
      return nms;
  }

  const vector<string> operator[](R_xlen_t i) const {
    if (is_list) {
      SEXP doc = VECTOR_ELT(corpus, i);
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
      const char* doc = CHAR(STRING_ELT(corpus, i));
      if (do_tokenize) {
        if (do_utf8) {
          if (is_ascii(doc))
            return tokenize(doc, rgx);
          else
            return wtokenize(doc, wrgx);
        } else {
          return tokenize(doc, rgx);
        }
      } else {
        return {string(doc)};
      }
    }
  }

};

#endif /* VOCAB_CORPUS_H */
