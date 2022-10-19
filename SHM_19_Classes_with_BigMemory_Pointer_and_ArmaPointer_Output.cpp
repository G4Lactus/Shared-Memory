#include <iostream>
// [[Rcpp::depends(RcppArmadillo, BH, bigmemory)]]
#include <RcppArmadillo.h>
#include <bigmemory/BigMatrix.h>

class External_bigMatrix_Administration {

public:
  
  // ctor
  External_bigMatrix_Administration (SEXP ptr_big_mat) {
    Rcpp::XPtr<BigMatrix> xpMat(ptr_big_mat);
    this->armaMat = new arma::Mat<double>( (double*)xpMat->matrix(), xpMat->nrow(), xpMat->ncol(), false, false);
  };
  
  // dtor
  ~External_bigMatrix_Administration() {
    delete armaMat;
  }
  
  // Member method
  void print_Matrix() const{
    armaMat->print();
    Rcpp::Rcout << std::endl;
  }
  
  void add_Matrix_to_itself() {
    *armaMat += *armaMat;
  }
  
  arma::mat return_Matrix() const {
    return *armaMat;
  }
  
private:
  // Attribute
  arma::mat* armaMat{nullptr};

};


RCPP_MODULE(mod_External_bigMatrix_Admin) {
  
  Rcpp::class_<External_bigMatrix_Administration>("External_bigMatrix_Administration")
    .constructor<SEXP>("Creates new class obj.")
    .method("print_Matrix", &External_bigMatrix_Administration::print_Matrix, "Prints matrix.")
    .method("add_Matrix_to_itself", &External_bigMatrix_Administration::add_Matrix_to_itself, "Add matrix to itself.")
    .method("return_Matrix", &External_bigMatrix_Administration::return_Matrix, "Return arma matrix obj.")
  ;
  
}

