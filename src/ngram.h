
 
#ifndef MAGISTRAL_VOCAB_NGRAM_H
#define MAGISTRAL_VOCAB_NGRAM_H

#include "common.h"

vector<string> wordgrams(const vector<string>& doc,
                         const size_t ngram_min,
                         const size_t ngram_max,
                         const string sep = "_");

vector<double> ngram_weights(const vector<double>& weights,
                             const size_t ngram_min,
                             const size_t ngram_max);
#endif
