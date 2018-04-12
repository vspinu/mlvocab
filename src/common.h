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

using namespace std;
using namespace Rcpp;

inline bool is_ascii(const char *str) {
  const char *p;
  for(p = str; *p; p++)
	if((unsigned int)*p > 0x7F) return FALSE;
  return TRUE;
}

 

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
typedef sparse_hash_map<string, uint32_t>::iterator shm_string_iter;
typedef sparse_hash_map<const char*, uint32_t>::iterator shm_char_iter;


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
   ROW,
   COL,
   BOTH             
  };


/// DBG

// adapted from https://stackoverflow.com/a/24118024/453735
// used to analyze pas by ref semantics
#define REFS(MSG,X) Rprintf("%20s SEXP=<%p>. OBJ=<%p>\n", MSG, (SEXP)X, &X ) ;

#endif /* MLVOCAB_H */
