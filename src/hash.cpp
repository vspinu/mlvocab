 
// api header from the digest package
// https://github.com/eddelbuettel/digest/blob/master/inst/include/pmurhashAPI.h
#include <pmurhashAPI.h>
#include "hash.h"
#include <cstring>

const MH_UINT32 MURMURHASH3_HASH_SEED = 3120602769LL;

uint32_t murmur3hash(const char* ptr) {
  return static_cast<uint32_t>(PMurHash32(MURMURHASH3_HASH_SEED, ptr, std::strlen(ptr)));
}

uint32_t murmur3hash(const std::string& str) {
  return static_cast<uint32_t>(PMurHash32(MURMURHASH3_HASH_SEED, str.c_str(), str.size()));
}
 
//[[Rcpp::export]]
SEXP murmur3hash(SEXP x) {
  // use raw R api here for sanity
  if (!Rf_isString(x))
    Rf_error("'x' must be a character string");
  R_xlen_t N = Rf_xlength(x);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, N));
  int* outptr = INTEGER(out);
  const char* ptr;
  for (auto i = 0; i < N; i++) {
    ptr = CHAR(STRING_ELT(x, i));
    /* outptr[i] = strlen(ptr); */
    outptr[i] = static_cast<int>(PMurHash32(MURMURHASH3_HASH_SEED, ptr, std::strlen(ptr)));
  }
  UNPROTECT(1);
  return out;
}
