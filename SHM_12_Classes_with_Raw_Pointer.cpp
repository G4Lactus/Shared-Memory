#include <iostream>
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>


class Matrix_Administration {
  
  public:
    
    // ctor
    // --------------------
    Matrix_Administration (arma::mat&& A, bool aux_mem) {
      this->armaMat = new arma::mat((double*)A.memptr(), A.n_rows, A.n_cols, aux_mem, false);
    };
    
    
    // dtor
    // --------------------
    ~Matrix_Administration() {
      delete armaMat;
    }

    // Member method
    // --------------------
    void print_Matrix() const{
      armaMat->print();
      Rcpp::Rcout << std::endl;
    }
    
    void retrieve_ptr_address() const {
      Rcpp::Rcout << armaMat->memptr() << std::endl;
    }
    
    arma::mat* add_42_to_Matrix() {
      *armaMat += 42;
      return armaMat;
    }
    
    arma::mat* take_Matrix_times_2() {
      *armaMat *= 2;
      return armaMat;
    }
    
    arma::mat* subtract_Minus_40() {
      *armaMat -= 40;
      return armaMat;
    }
    
    arma::mat* divide_by_5() {
      *armaMat /= 5;
      return armaMat;
    }
    
    arma::mat* add_Matrix_to_itself() {
      *armaMat += *armaMat;
      return armaMat;
    }
    
    Rcpp::XPtr<arma::mat> raw_to_xptr() const {
      return Rcpp::XPtr<arma::mat>(armaMat);
    }
    
  private:
    // Attribute
    // --------------------
    arma::mat* armaMat{nullptr};

};


// [[Rcpp::export]]
Rcpp::XPtr<Matrix_Administration> Test_Matrix_Administration(arma::mat A, bool aux_mem) {

  Matrix_Administration* MA = new Matrix_Administration(std::move(A), aux_mem);
  MA->print_Matrix();
  MA->add_42_to_Matrix();
  MA->print_Matrix();
  MA->take_Matrix_times_2();
  MA->print_Matrix();
  MA->subtract_Minus_40();
  MA->print_Matrix();
  MA->divide_by_5();
  MA->print_Matrix();
  MA->add_Matrix_to_itself();
  MA->print_Matrix();
  MA->retrieve_ptr_address();

  return Rcpp::XPtr<Matrix_Administration> (MA);
}

// [[Rcpp::export]]
void reload_pointer_data_for_Matrix_Administration(Rcpp::XPtr<Matrix_Administration> xptrMA) {
  xptrMA->print_Matrix();
  xptrMA->subtract_Minus_40();
  xptrMA->print_Matrix();
  return;
}



RCPP_MODULE(mod_Matrix_Admin) {

  Rcpp::class_<Matrix_Administration>("Matrix_Administration")
    .constructor<arma::mat, bool>("Creates new class obj.")
    .method("print_Matrix", &Matrix_Administration::print_Matrix, "Prints matrix.")
    .method("add_42_to_Matrix", &Matrix_Administration::add_42_to_Matrix, "Add 42 to matrix.")
    .method("retrieve_ptr_address", &Matrix_Administration::retrieve_ptr_address, "Retrieve address of raw ptr.")
  ;

}
