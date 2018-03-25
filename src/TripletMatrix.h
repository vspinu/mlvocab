
 
#ifndef MLVOCAB_TRIPLET_MATRIX_H
#define MLVOCAB_TRIPLET_MATRIX_H


#include "common.h"

class TripletMatrix {

 public:

  // for Matrix compatibility 
  int nrow;
  int ncol;
  sparse_hash_map<uint_fast64_t, double>  vals;

  TripletMatrix():
    nrow(0), ncol(0) {};

  TripletMatrix(uint32_t nrow, uint32_t ncol):
    nrow(nrow), ncol(ncol) {};

  void add(uint32_t i, uint32_t j, double d, bool iprimary) {
    if (iprimary)
      vals[to64(i, j)] += d;
    else
      vals[to64(j, i)] += d;
  };

  size_t size() {
    return(vals.size());
  }
  
  void clear() {
    vals.clear();
  };

  // C++ standard doesn't allow specialization of member function without
  // specializing the containing class. Solution is to either implement a
  // verbose helper struct or a switch style dispatch.
  // https://stackoverflow.com/questions/2097811/c-syntax-for-explicit-specialization-of-a-template-function-in-a-template-clas

  SEXP get(const MatrixType& mattype, Nullable<const CharacterVector&> inames, Nullable<const CharacterVector&> jnames) {

    switch(mattype) {
     case MatrixType::DGT:
       return dgTMatrix(inames, jnames);
     case MatrixType::DGC:
       return dg_Matrix(inames, jnames, true);
     case MatrixType::DGR:
       return dg_Matrix(inames, jnames, false);
     default:
       Rf_error("Invalid Matrix type");
    }
  };

 private:

  
  // LOW/HIGH bits of the hash key
  
  inline uint_fast64_t to64(uint_fast32_t i, uint_fast32_t j) {
    return(static_cast<uint_fast64_t>(i) << 32 | j);
  }

  inline int first32(uint_fast64_t x) {
    return static_cast<int>(x >> 32); // high 32 bits
  }

  inline int second32(uint_fast64_t x) {
    return static_cast<int>(x); // low 32 bits
  }


  
  // EXPORT FUNCTIONS
  
  S4 dgTMatrix(Nullable<const CharacterVector&> rownames, Nullable<const CharacterVector&> colnames) {

    int nrow = std::max(this->nrow, LENGTH(rownames.get()));
    int ncol = std::max(this->ncol, LENGTH(colnames.get()));
    
    size_t nnz = size();
    IntegerVector I(nnz), J(nnz);
    NumericVector X(nnz);
    
    size_t i = 0;
    for(const auto& v : vals) {
      I[i] = first32(v.first);
      J[i] = second32(v.first);
      X[i] = v.second;
      i++;
    }

    S4 out("dgTMatrix");
    out.slot("i") = I;
    out.slot("j") = J;
    out.slot("x") = X;
    out.slot("Dim") = IntegerVector::create(nrow, ncol);
    // fixme: replace with nullable
    out.slot("Dimnames") = List::create(rownames.get(), colnames.get());
    return out;
  }

  S4 dg_Matrix (Nullable<const CharacterVector&> rownames, Nullable<const CharacterVector&> colnames, bool C = true) {

    // see the doc entry CsparseMatrix for internals of dgCMatrix
    
    int nrow = std::max(this->nrow, LENGTH(rownames.get()));
    int ncol = std::max(this->ncol, LENGTH(colnames.get()));

    int jsize = C ? ncol : nrow;

    int psize = jsize + 1;
    IntegerVector P(psize);
     
    size_t nnz = size();
    vector<vector<int>> vvi(jsize);
    vector<vector<double>> vvx(jsize);
    for (const auto& v : vals) {
      int i = C ? first32(v.first) : second32(v.first);
      int j = C ? second32(v.first) : first32(v.first);
      vvi[j].push_back(i);
      vvx[j].push_back(v.second);
      P[j + 1]++;
    }

    for (int i = 1; i < psize; i++)
      P[i] += P[i-1];

    IntegerVector I(nnz);
    NumericVector X(nnz);

    for (int i = 0; i < jsize; i++) {
      vector<size_t> perm = sorting_permutation<int>(vvi[i]);
      vector<int> ivec = apply_permutation<int>(vvi[i], perm);
      vector<double> xvec = apply_permutation<double>(vvx[i], perm);
      int pbeg = P[i];
      /* Rprintf("pbeg:%d ivec:%d xvec %d\n", pbeg, ivec.size(), xvec.size()); */
      copy(ivec.begin(), ivec.end(), I.begin() + pbeg);
      copy(xvec.begin(), xvec.end(), X.begin() + pbeg);
    }

    S4 out(C ? "dgCMatrix" : "dgRMatrix");
    out.slot(C ? "i" : "j") = I;
    out.slot("p") = P;
    out.slot("x") = X;
    out.slot("Dim") = IntegerVector::create(nrow, ncol);
    out.slot("Dimnames") = List::create(rownames.get(), colnames.get());
    return out;
  }

  
  // SORT TWO VECTORS SIMULTANEOUSLY
  // adapted from http://stackoverflow.com/a/17074810/453735
  template <typename T>
  std::vector<size_t> sorting_permutation(const vector<T>& v) {
    std::vector<size_t> p(v.size());
    std::iota(p.begin(), p.end(), 0);
    std::sort(p.begin(), p.end(),
              [&](size_t i, size_t j){ return v[i] < v[j]; });
    return p;
  }

  template <typename T>
  std::vector<T> apply_permutation(const std::vector<T>& vec, const std::vector<std::size_t>& p) {
    std::vector<T> sorted_vec(vec.size());
    std::transform(p.begin(), p.end(), sorted_vec.begin(),
                   [&](std::size_t i){ return vec[i]; });
    return sorted_vec;
  }
  
};


#endif
