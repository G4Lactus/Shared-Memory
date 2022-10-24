#include <iostream>
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>


class Matrix_Administration {
  
  public:
    
    // ctor
    // --------------------
    Matrix_Administration (arma::mat A, bool aux_mem) {
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
      return;
    }
    
    void retrieve_armaMat_ptr_address() const {
      Rcpp::Rcout << armaMat->memptr() << std::endl;
      return;
    }
    
    void add_42_to_Matrix() {
      *armaMat += 42;
      return;
    }

    void take_Matrix_times_2() {
      *armaMat *= 2;
      return;
    }

    void subtract_Minus_40() {
      *armaMat -= 40;
      return;
    }

    void divide_Matrix_by_5() {
      *armaMat /= 5;
      return;
    }

    void add_Matrix_to_itself() {
      *armaMat += *armaMat;
      return;
    }

    Rcpp::XPtr<arma::mat> raw_to_xptr() const {
      return Rcpp::XPtr<arma::mat>(armaMat, true);
    }

    Rcpp::XPtr<Matrix_Administration> return_class_to_xptr() {
      return Rcpp::XPtr<Matrix_Administration>(this, true);
    }

  private:
    // Attribute
    // --------------------
    arma::mat* armaMat{nullptr};

};


// [[Rcpp::export]]
Rcpp::XPtr<Matrix_Administration> test_Matrix_Administration(arma::mat A, bool aux_mem) {

  Matrix_Administration* MA = new Matrix_Administration(std::move(A), aux_mem);
  MA->print_Matrix();
  MA->add_42_to_Matrix();
  MA->print_Matrix();
  MA->take_Matrix_times_2();
  MA->print_Matrix();
  MA->subtract_Minus_40();
  MA->print_Matrix();
  MA->divide_Matrix_by_5();
  MA->print_Matrix();
  MA->add_Matrix_to_itself();
  MA->print_Matrix();
  MA->retrieve_armaMat_ptr_address();

  return Rcpp::XPtr<Matrix_Administration> (MA);
}

// [[Rcpp::export]]
void reload_pointer_data_for_Matrix_Administration(Rcpp::XPtr<Matrix_Administration> xptrMA) {
  xptrMA->print_Matrix();
  xptrMA->subtract_Minus_40();
  xptrMA->print_Matrix();
  
  // let's create a new class instance based on default copy constructor
  Matrix_Administration MA2 = *xptrMA.get();
  MA2.print_Matrix();
  return;
}



RCPP_MODULE(mod_Matrix_Admin) {

  Rcpp::class_<Matrix_Administration>("Matrix_Administration")
    .constructor<arma::mat, bool>("Creates new class obj.")
    .method("print_Matrix", &Matrix_Administration::print_Matrix, "Prints matrix.")
    .method("retrieve_ptr_address", &Matrix_Administration::retrieve_armaMat_ptr_address, "Retrieve address of raw ptr.")
    .method("add_42_to_Matrix", &Matrix_Administration::add_42_to_Matrix, "Add 42 to matrix.")
    .method("take_Matrix_times_2", &Matrix_Administration::take_Matrix_times_2, "Multiply matrix elements with 2.")
    .method("subtract_Minus_40", &Matrix_Administration::subtract_Minus_40, "Subtract 40 from each matrix element.")
    .method("divide_Matrix_by_5", &Matrix_Administration::divide_Matrix_by_5, "Divide each matrix element by 5.")
    .method("add_Matrix_to_itself", &Matrix_Administration::add_Matrix_to_itself, "Add matrix to itself.")
    .method("raw_to_xptr", &Matrix_Administration::raw_to_xptr, "Return external pointer to classes' armaMat.")
    .method("return_class_to_xptr", &Matrix_Administration::return_class_to_xptr, "Return external pointer to class instance.")
  ;

}
