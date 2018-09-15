#ifndef MLVOCAB_PRISEC_MATRIX_H
#define MLVOCAB_PRISEC_MATRIX_H

#include "common.h"
#include <atomic>

class PriSecMatrix {

  atomic<int> nnz {0};
  atomic<int> ncompacted {0};
  atomic<int> npri;
  atomic<int> nsec;

 public:

  vector<vector<double>> vals;
  vector<vector<int>> ixes;

  PriSecMatrix(int npri) : PriSecMatrix(npri, 0) {};

  PriSecMatrix(int npri, int nsec):
    npri(npri), nsec(nsec), vals(npri), ixes(npri) {
  };

  inline void alloc(int i, size_t new_size) {
    ixes[i].reserve(new_size);
    vals[i].reserve(new_size);
  }

  inline void set(size_t i, size_t j, double d) {
    ixes[i].push_back(j);
    vals[i].push_back(d);
  };

  void sort(size_t i) {
    vector<size_t> perm = sorting_permutation(ixes[i]);
    vector<double>& vs = vals[i];
    vector<int>& ix = ixes[i];
    apply_permutation_in_place(vs, perm);
    apply_permutation_in_place(ix, perm);
  }


  // must be called once per doc
  void sort_and_compact(size_t i) {
    vector<double>& vs = vals[i];
    vector<int>& ix = ixes[i];
    if (ix.size() > 0) {
      vector<size_t> perm = sorting_permutation(ix);
      apply_permutation_in_place(vs, perm);
      apply_permutation_in_place(ix, perm);
      compact_sorted(vs, ix);
      // atomic add
      nnz.fetch_add(vs.size(), memory_order_relaxed);
      // atomic max
      const int loc_nsec = ix[ix.size()-1] + 1;
      int prev_nsec = nsec;
      while(prev_nsec < loc_nsec && !nsec.compare_exchange_weak(prev_nsec, loc_nsec));
    }
    // atomic add
    ncompacted.fetch_add(1, memory_order_relaxed);
  }

  // C++ standard doesn't allow specialization of member function without
  // specializing the containing class. Solution is to either implement a
  // verbose helper struct or a switch style dispatch.
  // https://stackoverflow.com/questions/2097811/c-syntax-for-explicit-specialization-of-a-template-function-in-a-template-clas
  SEXP get(const MatrixType& mattype,
           Nullable<const CharacterVector&> prinames,
           Nullable<const CharacterVector&> secnames,
           bool symmetric = false,
           bool pri_as_rows = true) {
    switch(mattype) {
     case MatrixType::DGT:
       return dTMatrix(prinames, secnames, symmetric, pri_as_rows);
     case MatrixType::DGC:
       return dMatrix(prinames, secnames, symmetric, pri_as_rows);
     case MatrixType::DGR:
       return dMatrix(prinames, secnames, symmetric, pri_as_rows);
     default:
       Rf_error("Invalid Matrix type");
    }
  };

  void transpose() {
    check_compactification();
    int new_nsec = npri, new_npri = nsec;
    vector<vector<double>> new_vals(new_npri);
    vector<vector<int>> new_ixes(new_npri);
    for (size_t i = 0; i < vals.size(); i++) {
      vector<int>& ix = ixes[i];
      for (size_t j = 0; j < ix.size(); j++) {
        new_ixes[ix[j]].push_back(i);
        new_vals[ix[j]].push_back(vals[i][j]);
      }
      vals[i].clear();
      ixes[i].clear();
    }
    npri.store(new_npri);
    nsec.store(new_nsec);
    ncompacted.store(new_npri);

    vals = new_vals;
    ixes = new_ixes;
    /* Rf_PrintValue(wrap(ixes[0])); */
    /* Rf_PrintValue(wrap(vals[0])); */
  }

  // col/row-wise multiplication
  void apply_weight(const NumericVector& weights, const MatrixDimType across) {
    if (across == MatrixDimType::PRIMARY || across == MatrixDimType::BOTH) {
      if (npri > static_cast<int>(weights.size()))
        Rf_error("Insufficient weights size (%d); required: %d", weights.size(), npri.load());
    }
    if (across == MatrixDimType::SECONDARY || across == MatrixDimType::BOTH) {
      if (nsec > static_cast<int>(weights.size()))
        Rf_error("Insufficient weights size (%d); required = %d", weights.size(), nsec.load());
    }

    if (across == MatrixDimType::SECONDARY)
      for (size_t i = 0; i < vals.size(); i++) {
        vector<int>& ix = ixes[i];
        for (size_t j = 0; j < ix.size(); j++) {
          vals[i][j] *= weights[ix[j]];
        }
      }
    else if (across == MatrixDimType::PRIMARY)
      for (size_t i = 0; i < vals.size(); i++) {
        double w = weights[i];
        for (double& v : vals[i]) {
          v *= w;
        }
      }
    else
      for (size_t i = 0; i < vals.size(); i++) {
        vector<int>& ix = ixes[i];
        double w = weights[i];
        for (size_t j = 0; j < ix.size(); j++) {
          vals[i][j] *= (weights[ix[j]] * w);
        }
      }
  }


 private:

  void check_compactification() {
    if (ncompacted.load() != npri.load())
      Rf_error("Internal error: Incomplete compactification (ncompacted: %d, npris: %d)",
               ncompacted.load(), npri.load());
  }

  
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

  S4 dTMatrix(Nullable<const CharacterVector&> prinames,
              Nullable<const CharacterVector&> secnames,
              bool symmetric,
              bool pri_as_rows) {

    check_compactification();

    int npri = std::max(this->npri.load(), prinames.isNull() ? 0 : LENGTH(prinames.get()));
    int nsec = std::max(this->nsec.load(), secnames.isNull() ? 0 : LENGTH(secnames.get()));

    IntegerVector I(nnz), J(nnz);
    NumericVector X(nnz);

    size_t oi = 0;
    for(size_t i = 0; i < vals.size(); i++) {
      vector<int> ix = ixes[i];
      for (size_t j = 0; j < ix.size(); j++) {
        I[oi] = i;
        J[oi] = ix[j];
        X[oi] = vals[i][j];
        oi++;
      }
    }

    string s4class = string("d") + (symmetric ? "s" : "g") + "TMatrix";
    S4 out(s4class);
    if (symmetric)
      out.slot("uplo") = "U";

    out.slot("x") = X;

    if (pri_as_rows) {
      out.slot("i") = I;
      out.slot("j") = J;
      out.slot("Dim") = IntegerVector::create(npri, nsec);
      out.slot("Dimnames") = List::create(prinames.get(), secnames.get());
    } else {
      out.slot("i") = J;
      out.slot("j") = I;
      out.slot("Dim") = IntegerVector::create(nsec, npri);
      out.slot("Dimnames") = List::create(secnames.get(), prinames.get());
    }
    return out;
  }

  S4 dMatrix (Nullable<const CharacterVector&> prinames,
              Nullable<const CharacterVector&> secnames,
              bool symmetric,
              bool pri_as_rows) {

    check_compactification();

    // see the doc entry CsparseMatrix for internals of dgCMatrix

    size_t npri = std::max(this->npri.load(), prinames.isNull() ? 0 : LENGTH(prinames.get()));
    size_t nsec = std::max(this->nsec.load(), secnames.isNull() ? 0 : LENGTH(secnames.get()));

    size_t psize = npri + 1;
    IntegerVector P(psize);
    for (size_t i = 0; i < npri; i++)
      if (i < vals.size())
        P[i+1] = P[i] + vals[i].size();
      else
        P[i+1] = P[i];

    IntegerVector I(nnz);
    NumericVector X(nnz);

    for (size_t i = 0; i < vals.size(); i++) {
      int pbeg = P[i];
      copy(ixes[i].begin(), ixes[i].end(), I.begin() + pbeg);
      copy(vals[i].begin(), vals[i].end(), X.begin() + pbeg);
    }

    string s4class = string("d") + (symmetric ? "s" : "g") + (pri_as_rows ? "R" : "C") + "Matrix";
    S4 out(s4class);
    if (symmetric)
      out.slot("uplo") = "U";

    out.slot("x") = X;
    out.slot("p") = P;


    if (pri_as_rows) {
      out.slot("j") = I;
      out.slot("Dim") = IntegerVector::create(npri, nsec);
      out.slot("Dimnames") = List::create(prinames.get(), secnames.get());
    } else {
      out.slot("i") = I;
      out.slot("Dim") = IntegerVector::create(nsec, npri);
      out.slot("Dimnames") = List::create(secnames.get(), prinames.get());
    }
    return out;
  }

};


#endif
