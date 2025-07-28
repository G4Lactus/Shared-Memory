// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>



// This helper function modifies an Armadillo matrix passed by reference
arma::mat modify_by_ref_ptr(arma::mat& A) {
 
  Rcpp::Rcout << "Inside C++: Modifying via a pointer to the received object\n";

  arma::mat Ac = A; // Copy input
  Ac += 42;         // modify copy
  return Ac;        // return modified copy
}


// This helper function demonstrates creating a NEW matrix in memory
// 1. Copies the content of A.
// 2. Allocates a new object on the heap.
// 3. Modifies the new object.
// 4. Returns a pointer to the new heap object.
arma::mat* modify_by_new_ptr(arma::mat& A) {
  Rcpp::Rcout << "Inside C++: Modifying a new object with 'new'\n";

  // WARNING: MEMORY LEAK
  // The 'new' keyword allocated fresh memory on teh heap that C++ will
  // not automatically manage. The memory is owned by the program, and
  // we are responsible for freeing with with 'delete' later on.
  // As no delete is called, a memory leak occurs.

  // new matrix on the heap, copying the data
  arma::mat* ptr_to_new_mat = new arma::mat((double*) A.memptr(), A.n_rows, A.n_cols);

  // modify content of the NEW matrix
  *ptr_to_new_mat += 100;

  // Return the pointer tot he newly allocated memory
  return ptr_to_new_mat;
}



// [[Rcpp::export]]
arma::mat simulate_pointer_behavior(arma::mat& A,int type) {

  switch (type) {
    case 1: {
      // Return a dereferenced pointer pointing towards A.
      // Note, the original A is not modified, we return a copy.
      return modify_by_ref_ptr(A);

    };
    case 2: {
      // Memory Leak:
      // we can't return the raw pointer, and have no way to
      // 'delete' it later. The memory is orphaned.
      return *modify_by_new_ptr(A);

    };
    default: {
      throw Rcpp::exception("Sorry type not defined, just 1 or 2.");
      break;
    };
  }
}