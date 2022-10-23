#include <iostream>

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// In the following we investigate raw pointers in Rcpp and R.

// have some data manipulation function not available from R, which returns a 
// raw pointer.
arma::mat* return_ArmaMat_as_rawPtr_sameObj(arma::mat& A) {
  
  A.print();
  // create a raw pointer and and let it point to A's reference pointer
  arma::mat* ptr_arma = &A;
  *ptr_arma += 42;
  ptr_arma->print();
  
  return ptr_arma;
}

arma::mat* return_ArmaMat_as_rawPtr_newObj(arma::mat& A) {
  
  A.print();
  // create a new raw pointer
  arma::mat* ptr_arma = new arma::mat((double*) A.memptr(), A.n_rows, A.n_cols);
  *ptr_arma += 42;
  ptr_arma->print();
  
  return ptr_arma;
}

typedef long int li;
// [[Rcpp::export]]
arma::mat simulate_rawPtr_Usage(arma::mat& A,li type) {
  switch (type) {
    case 1:
      {
        return *return_ArmaMat_as_rawPtr_sameObj(A);                
      };
    case 2:
      {
        return *return_ArmaMat_as_rawPtr_newObj(A);        
      };
    default:
      {
        throw Rcpp::exception("Sorry type not defined, just 1 or 2.");
      };
  }
}
// NOTE: returning a raw ptr to R fails it is no SEXP (S expression).
// The code does not compile! Either you dereference the pointer or you make
// an XPtr and return it to R.
// arma::mat* simulate_rawPtr_Usage(arma::mat& A) {
//   return return_rawPtr_to_armaMat(A);
// }
