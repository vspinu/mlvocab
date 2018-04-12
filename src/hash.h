 
#ifndef MAGISTRAL_VOCAB_HASH_H
#define MAGISTRAL_VOCAB_HASH_H

#include <cstdint>
#include <string>
#include <Rinternals.h>

uint32_t murmur3hash(const char* ptr);
uint32_t murmur3hash(const std::string &str);
SEXP murmur3hash(SEXP x, int nbuckets);

#endif // MAGISTRAL_VOCAB_HASH_H
