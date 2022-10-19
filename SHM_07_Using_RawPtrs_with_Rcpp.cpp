#include <iostream>

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// In the following we investigate raw pointers in Rcpp and R.

// have some data manipulation function not available from R
arma::mat* return_ArmaMat_as_rawPtr(arma::mat& A) {
  
  Rcpp::Rcout << "Inner part of the pipe." << std::endl;
  
  // create a new raw pointer and transfer data to the heap
  arma::mat* ptr_arma = new arma::mat((double*) A.memptr(), A.n_rows, A.n_cols);
  *ptr_arma += 42;
  
  for (std::size_t i{0}; i < ptr_arma->n_cols; ++i) {
    for (std::size_t j{0}; j < ptr_arma->n_rows; ++j) {
      Rcpp::Rcout << ptr_arma->at(j, i) << std::endl;
    }
  }
  Rcpp::Rcout << "Inner part finished." << std::endl;
  return ptr_arma; // fct returns a ptr, see * in return data type
}

// The naive approach to return a raw pointer to R fails!
// It is no SEXP (S expression). The code does not compile!

// [[Rcpp::export]]
// arma::mat* return_rawPtr_to_A(arma::mat& A) {
//   return return_ArmaMat_as_rawPtr(A);
// }

// The solution is to dereference the pointer first, to return a stack obj.
arma::mat return_rawPtr_to_A(arma::mat& A) {
  Rcpp::Rcout << "Return value\n";
  return *return_ArmaMat_as_rawPtr(A);
}


// lets build a pipeline, not accessible from R
arma::mat return_armaMat_after_pipeline_to_R(arma::mat& A) {
  
  Rcpp::Rcout << "Start of the pipeline." << std::endl;
  
  // Armadillo mat objs can be set as raw ptr objs, but don't need explicit
  // delete: results into a crash
  arma::mat* B = return_ArmaMat_as_rawPtr(A);
  (*B).print("Copied matrix");
  // no explicit delete for B!
  Rcpp::Rcout << "\n";
  
  for (std::size_t i{0}; i < return_ArmaMat_as_rawPtr(A)->n_cols; ++i) {
    for (std::size_t j{0}; j < return_ArmaMat_as_rawPtr(A)->n_rows; ++j) {
      // now we work with the raw ptr, and for every (j, i) combination
      // a new run of return_ArmaMat_as_rawPtr is executed.
      Rcpp::Rcout << (*return_ArmaMat_as_rawPtr(A))(j, i) << std::endl;
    }
  }
  
  Rcpp::Rcout << "End of pipepline." << std::endl;
  // before returning to R, de-reference your raw pointer
  return *(return_ArmaMat_as_rawPtr(A));
}


// [[Rcpp::export]]
arma::mat return_A(arma::mat& A) {
  return return_armaMat_after_pipeline_to_R(A);
}