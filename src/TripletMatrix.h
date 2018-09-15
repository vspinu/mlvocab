

#ifndef MLVOCAB_TRIPLET_MATRIX_H
#define MLVOCAB_TRIPLET_MATRIX_H


#include "common.h"

class TripletMatrix {

 public:

  int nrow;
  int ncol;

  hashmap<uint_fast64_t, double> vals;

  TripletMatrix():
    nrow(0), ncol(0) {};

  TripletMatrix(int nrow, int ncol):
    nrow(nrow), ncol(ncol) {};

  inline void add(uint32_t i, uint32_t j, double d) {
      vals[to64(i, j)] += d;
  };

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

  SEXP get(const MatrixType& mattype,
           Nullable<const CharacterVector&> inames,
           Nullable<const CharacterVector&> jnames,
           bool symmetric = false) {
    switch(mattype) {
     case MatrixType::DGT:
       return dTMatrix(inames, jnames, symmetric);
     case MatrixType::DGC:
       return dMatrix(inames, jnames, true, symmetric);
     case MatrixType::DGR:
       return dMatrix(inames, jnames, false, symmetric);
     default:
       Rf_error("Invalid Matrix type");
    }
  };

  // col/row-wise multiplication
  void apply_weight(const NumericVector& weights, const MatrixDimType across) {
    if (across == MatrixDimType::PRIMARY || across == MatrixDimType::BOTH) {
      if (nrow > static_cast<int>(weights.size()))
        Rf_error("Insufficient weights size (%d); nrow = %d", weights.size(), nrow);
    }
    if (across == MatrixDimType::SECONDARY || across == MatrixDimType::BOTH) {
      if (ncol > static_cast<int>(weights.size()))
        Rf_error("Insufficient weights size (%d); nrow = %d", weights.size(), ncol);
    }

    if (across == MatrixDimType::PRIMARY)
      for (auto& v : vals) {
        int rix = first32(v.first);
        v.second *= weights[rix];
      }
    else if (across == MatrixDimType::SECONDARY)
      for (auto& v : vals) {
        int cix = second32(v.first);
        v.second *= weights[cix];
      }
    else
      for (auto& v : vals) {
        int rix = first32(v.first);
        int cix = second32(v.first);
        v.second *= (weights[cix] * weights[rix]);
      }
  }


 private:

  
  // LOW/HIGH bits of the hash key

  inline uint_fast64_t to64(uint32_t i, uint32_t j) {
    return(static_cast<uint_fast64_t>(i) << 32 | j);
  }

  inline int first32(uint_fast64_t x) {
    return static_cast<int>(x >> 32); // high 32 bits
  }

  inline int second32(uint_fast64_t x) {
    return static_cast<int>(x); // low 32 bits
  }


  
  // EXPORT FUNCTIONS

  S4 dTMatrix(Nullable<const CharacterVector&> rownames,
              Nullable<const CharacterVector&> colnames,
              bool symmetric) {

    int nrow = std::max(this->nrow, rownames.isNull() ? 0 : LENGTH(rownames.get()));
    int ncol = std::max(this->ncol, colnames.isNull() ? 0 : LENGTH(colnames.get()));

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

    string s4class = string("d") + (symmetric ? "s" : "g") + "TMatrix";
    S4 out(s4class);
    if (symmetric)
      out.slot("uplo") = "U";
    out.slot("i") = I;
    out.slot("j") = J;
    out.slot("x") = X;
    out.slot("Dim") = IntegerVector::create(nrow, ncol);
    // fixme: replace with nullable
    out.slot("Dimnames") = List::create(rownames.get(), colnames.get());
    return out;
  }

  S4 dMatrix (Nullable<const CharacterVector&> rownames, Nullable<const CharacterVector&> colnames,
              bool C, bool symmetric) {

    // see the doc entry CsparseMatrix for internals of dgCMatrix

    int nrow = std::max(this->nrow, rownames.isNull() ? 0 : LENGTH(rownames.get()));
    int ncol = std::max(this->ncol, colnames.isNull() ? 0 : LENGTH(colnames.get()));

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

    string s4class = string("d") + (symmetric ? "s" : "g") + (C ? "C" : "R") + "Matrix";
    S4 out(s4class);
    if (symmetric)
      out.slot("uplo") = "U";
    out.slot(C ? "i" : "j") = I;
    out.slot("p") = P;
    out.slot("x") = X;
    out.slot("Dim") = IntegerVector::create(nrow, ncol);
    out.slot("Dimnames") = List::create(rownames.get(), colnames.get());
    return out;
  }

};


#endif
