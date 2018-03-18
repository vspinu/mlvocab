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

#include "ngram.h"
 
vector<string> wordgrams(const vector<string>& doc,
                         const size_t ngram_min,
                         const size_t ngram_max,
                         const string sep) {

  if (ngram_min == ngram_max && ngram_max == 1)
    return(doc);

  size_t len = doc.size();
  vector<string> ngrams;
  ngrams.reserve(len);
  string kgram;
      
  for (size_t i = 0; i < len; i++) {
    size_t k = 1, j = i;
    while (k <= ngram_max && j < len) {
      if (k == 1) {
        kgram = doc[j];
      } else {
        kgram.append(sep);
        kgram.append(doc[j]);
      }
      if(k >= ngram_min) {
        ngrams.push_back(kgram);
      }
      j++;
      k++;
    }
  }
  return(ngrams);
}
