// [[Rcpp::depends(RcppArmadillo)]]
#include <iostream>
#include <memory>
#include <RcppArmadillo.h>


class External_Matrix_Administration {
  
public:
  
  // ctor
  // --------------------
  External_Matrix_Administration (Rcpp::XPtr<arma::mat> xptr_A) {
    Rcpp::Rcout << "XPtr from R: " << xptr_A << "\n";
    Rcpp::Rcout << *xptr_A << "\n";
    this->armaMat = std::unique_ptr<arma::mat>(xptr_A);
    Rcpp::Rcout << "Smart pointer in C++ taking the address: " << armaMat.get() << "\n";
    Rcpp::Rcout << *armaMat << "\n";
  };
  
  // dtor
  // --------------------
  ~External_Matrix_Administration() = default;

  // Member method
  // --------------------
  void print_Matrix() const{
    armaMat->print();
    Rcpp::Rcout << std::endl;
  }
  
  void add_42_to_Matrix() {
    *armaMat += 42;
  }
  
  void add_number_to_Matrix(const double number) {
    *armaMat += number;
  }

  arma::mat return_Matrix() const{
    return *armaMat;
  }
  
private:
  // Attribute
  // --------------------
  std::unique_ptr<arma::mat> armaMat{nullptr};

};


Rcpp::XPtr<arma::mat> create_XPtr_for_R_obj(arma::mat& A) {
  arma::mat* armaMat = new arma::mat(A.memptr(), A.n_rows, A.n_cols, true, false);
  return Rcpp::XPtr<arma::mat> (armaMat);
}


RCPP_MODULE(mod_External_Matrix_Admin) {
  using namespace Rcpp;

  function("create_XPtr_for_R_obj",
           &create_XPtr_for_R_obj,
           List::create(_["mat"]), "Create external pointer of R matrix obj");
  

  Rcpp::class_<External_Matrix_Administration>("External_Matrix_Administration")
   .constructor<Rcpp::XPtr<arma::mat>>("Creates new class obj.")
   .method("print_Matrix", &External_Matrix_Administration::print_Matrix, "Prints matrix.")
   .method("add_42_to_Matrix", &External_Matrix_Administration::add_42_to_Matrix, "Add 42 to matrix.")
   .method("return_Matrix", &External_Matrix_Administration::return_Matrix, "Return matrix to R.")
   .method("add_number_to_Matrix", &External_Matrix_Administration::add_number_to_Matrix, "Add number to Matrix.")
  ;
  
}