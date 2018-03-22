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

#endif /* MLVOCAB_H */
