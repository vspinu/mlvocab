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
// mlvocab.  If not, see <http://www.gnu.org/licenses/>.

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

  // 1) "a b c d e f" with ngram_min = 1, ngram_max = 3 becomes:
  //    a a_b a_b_c b b_c b_c_d c c_d c_d_e d d_e d_e_f e e_f f
  // 2) "a b c d e f" with ngram_min = 2, ngram_max = 3 becomes:
  //    a_b a_b_c b_c b_c_d c_d c_d_e d_e d_e_f e_f
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

vector<double> ngram_weights(const vector<double>& weights,
                             const size_t ngram_min,
                             const size_t ngram_max) {

  /* if (ngram_min == ngram_max && ngram_max == 1) */
  /*   return(weights); */

  size_t wlen = weights.size();
  vector<double> new_weights, n_weights;
  new_weights.reserve(wlen);
  n_weights.reserve(wlen);
  double kweight, n;
  
  /*
    Each multigram proximity is an average of 1-gram proximities:
    
    mlvocab:::ngram_weights(letters[1:5], ngram_min = 1, ngram_max = 3)
       a   a_b a_b_c     b   b_c b_c_d     c   c_d c_d_e     d   d_e     e 
    1.00  0.75  0.61  0.50  0.42  0.36  0.33  0.29  0.26  0.25  0.22  0.20 
 
    mlvocab:::ngram_weights(letters[1:5], ngram_min = 2, ngram_max = 3)
     a_b a_b_c   b_c b_c_d   c_d c_d_e   d_e 
    0.75  0.61  0.42  0.36  0.29  0.26  0.22 
  */
  for (size_t i = 0; i < wlen; i++) {
    size_t k = 1, j = i;
    while (k <= ngram_max) {
      if (k == 1) {
        if (j >= wlen) break;
        kweight = weights[j];
        n = 1.0;
      } else {
        if (j < wlen)
          kweight += weights[j];
        n++;
      }
      if(k >= ngram_min) {
        new_weights.push_back(kweight/n);
      }
      j++;
      k++;
    }
  }
  return new_weights;
}
