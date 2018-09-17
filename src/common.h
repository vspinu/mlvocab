// Copyright (C) 2018  Vitalie Spinu
// This file is part of text2vec
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

#ifndef MLVOCAB_H
#define MLVOCAB_H

#include <Rcpp.h>
#include <stdio.h>
#include <string>
#include <vector>
#include <cstdint>

/* #define USE_RINTERNALS 1 // slight increase in speed */

using namespace Rcpp;
using namespace std;

inline bool is_ascii(const char *str) {
  const char *p;
  for(p = str; *p; p++)
	if((unsigned int)*p > 0x7F) return false;
  return true;
}

inline SEXP toRstrvec(const vector<string>& vec) {
  R_xlen_t N = vec.size();
  SEXP out = PROTECT(Rf_allocVector(STRSXP, N));
  for (R_xlen_t i = 0; i < N; i++) {
    SET_STRING_ELT(out, i, Rf_mkCharCE(vec[i].c_str(), CE_UTF8));
  }
  UNPROTECT(1);
  return out;
}


/// SPARSE HASH MAP

#define USE_SPP

#ifdef USE_SPP
#include <sparsepp/spp.h>
/// comment from text2vec:
// spp has calls to 'exit' on failure, which upsets R CMD check.
// We won't bump into them during normal test execution so just override
// it in the spp namespace before we include 'sparsepp'.
// https://github.com/hadley/testthat/blob/c7e8330867645c174f9a286d00eb0036cea78b0c/inst/include/testthat/testthat.h#L44-L50
// https://stackoverflow.com/questions/43263880/no-ambiguous-reference-error-even-after-using-namespace-directive/43294812
namespace spp {
inline void exit(int status) throw() {}
}
typedef spp::sparse_hash_map<string, uint32_t>::iterator hashmap_string_iter;
typedef spp::sparse_hash_map<const char*, uint32_t>::iterator hashmap_char_iter;
template<typename T1, typename T2>
using hashmap = spp::sparse_hash_map<T1, T2>;
#else
#include <unordered_map>
typedef std::unordered_map<string, uint_fast32_t>::iterator hashmap_string_iter;
typedef std::unordered_map<const char*, uint_fast32_t>::iterator hashmap_char_iter;
template<typename T1, typename T2>
using hashmap = std::unordered_map<T1, T2>;
#endif


/// GENERIC MATRIX TYPES
enum class MatrixType
  {
   BASE,
   DGT,
   DGC,
   DGR,
   DF,
   SIMPLE
  };


/// WINDOW CONTEXT TYPES
enum class ContextType
  {
   SYMMETRIC,
   RIGHT,
   LEFT
  };


/// DIMMENSION TYPE
enum class MatrixDimType
  {
   PRIMARY,
   SECONDARY,
   BOTH
  };


  
// SORT TWO VECTORS SIMULTANEOUSLY
// adapted from http://stackoverflow.com/a/17074810/453735
template <typename T>
std::vector<size_t> sorting_permutation(const vector<T>& v) {
  std::vector<size_t> p(v.size());
  std::iota(p.begin(), p.end(), 0);
  std::sort(p.begin(), p.end(),
            [&](size_t i, size_t j){ return v[i] < v[j];});
  return p;
}

template <typename T>
std::vector<T> apply_permutation(const std::vector<T>& vec, const std::vector<std::size_t>& p) {
  std::vector<T> sorted_vec(vec.size());
  std::transform(p.begin(), p.end(), sorted_vec.begin(),
                 [&](std::size_t i){ return vec[i]; });
  return sorted_vec;
}

template <typename T>
void apply_permutation_in_place(std::vector<T>& vec, const std::vector<std::size_t>& p) {
  std::vector<bool> done(vec.size());
  for (std::size_t i = 0; i < vec.size(); ++i) {
    if (done[i])
      continue;
    done[i] = true;
    std::size_t prev_j = i;
    std::size_t j = p[i];
    while (i != j) {
      std::swap(vec[prev_j], vec[j]);
      done[j] = true;
      prev_j = j;
      j = p[j];
    }
  }
}

template <typename T>
void compact_sorted(vector<T>& vec, vector<int>& ixs) {
  if (vec.size() != ixs.size())
    throw std::runtime_error("vec and ixs sizes must agree");
  int prev_ix = -1, real_i = -1;
  for (size_t i = 0; i < ixs.size(); ++i) {
    // real_i is always <= i
    if (prev_ix == ixs[i]) {
      vec[real_i] += vec[i];
    } else {
      real_i++;
      vec[real_i] = vec[i];
      ixs[real_i] = ixs[i];
      prev_ix = ixs[i];
    }
  }
  real_i++;
  vec.resize(real_i);
  ixs.resize(real_i);
}



/// DBG

// adapted from https://stackoverflow.com/a/24118024/453735
// used to analyze pas by ref semantics
#define REFS(MSG,X) Rprintf("%20s SEXP=<%p>. OBJ=<%p>\n", MSG, (SEXP)X, &X ) ;

#endif /* MLVOCAB_H */
