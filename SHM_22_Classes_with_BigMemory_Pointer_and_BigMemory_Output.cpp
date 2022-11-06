// [[Rcpp::depends(RcppArmadillo, BH, bigmemory)]]
#include <iostream>
#include <iomanip>
#include <string>

#include <RcppArmadillo.h>
#include <bigmemory/BigMatrix.h>
#include <bigmemory/MatrixAccessor.hpp>



class External_bigMatrix_Administration {

public:
  
  // ctor
  // ------------------
  External_bigMatrix_Administration (SEXP ptr_big_mat) {
    this->p_bigM = ptr_big_mat;
  };

  // dtor
  // ------------------
  ~External_bigMatrix_Administration() = default;


  // Member methods
  // ------------------
  void get_head_bigMatrix(std::size_t num_rows = 3) {
    
    const std::size_t field_width1{15}; // column width
    
    MatrixAccessor<double> macc = convert_bigMptr_to_MatrixAccessor(p_bigM) ;
    
    if (num_rows > std::size_t(macc.nrow())) {
      num_rows = std::size_t( ((macc.nrow() > 5) ? 5 : macc.nrow()) );
    }
    std::cout << std::setprecision(6) << std::fixed;
    
    std::string col_name{""};
    for (std::size_t j{1}; j <= std::size_t(macc.ncol()); ++j) {
      col_name = "[," + std::to_string(j) + "]";
      Rcpp::Rcout << std::right << std::setfill(' ') << std::setw(19) << col_name;
    }
    Rcpp::Rcout << std::endl;
    
    std::string row_name{""};
    for (std::size_t i{1}; i <= num_rows; ++i) {
      row_name = "[" + std::to_string(i) + ",]";
      for (std::size_t j{1}; j <= std::size_t(macc.ncol()); ++j) {
        Rcpp::Rcout << std::left
        << ((j == 1) ? row_name : "")
        << std::setw(field_width1 + 3) << std::right << std::to_string(macc[j-1][i-1]);
      }
      Rcpp::Rcout << std::endl;
    }
    
    Rcpp::Rcout << std::endl;
  }
  
  
  Rcpp::NumericVector colSums_bigMatrix() {
    
    MatrixAccessor<double> macc = convert_bigMptr_to_MatrixAccessor(p_bigM);
    
    Rcpp::NumericVector res(macc.ncol());
    for (std::size_t j{0}; j < std::size_t(macc.ncol()); ++j) {
      for (std::size_t i{0}; i < std::size_t(macc.nrow()); ++i) {
        res[j] += macc[j][i];
      }
    }
    
    return res;

  }
  
  
  Rcpp::NumericVector rowSums_bigMatrix() {
    
    MatrixAccessor<double> macc = convert_bigMptr_to_MatrixAccessor(p_bigM);
    
    Rcpp::NumericVector res(macc.nrow());
    for (std::size_t i{0}; i < std::size_t(macc.nrow()); ++i) {
      for (std::size_t j{0}; j < std::size_t(macc.ncol()); ++j) {
        res[i] += macc[j][i];
      }
    }
    
    return res;
    
  }
  
  
  Rcpp::NumericVector colDifference_bigMatrix() {
    
    MatrixAccessor<double> macc = convert_bigMptr_to_MatrixAccessor(p_bigM);
    
    Rcpp::NumericVector res(macc.ncol());
    for (std::size_t j{0}; j < std::size_t(macc.ncol()); ++j) {
      for (std::size_t i{0}; i < std::size_t(macc.nrow()); ++i) {
        res[j] -= macc[j][i];
      }
    }
    
    return res;
    
  }
  
  
  void multiply_scalar_with_bigMatrix(const double number) {
    
    MatrixAccessor<double> macc = convert_bigMptr_to_MatrixAccessor(p_bigM);
    
    Rcpp::NumericVector res(macc.ncol());
    for (std::size_t j{0}; j < std::size_t(macc.ncol()); ++j) {
      for (std::size_t i{0}; i < std::size_t(macc.nrow()); ++i) {
        macc[j][i] *= number;
      }
    }
    
  }
  
  
  void add_number_to_bigMatrix(const double number) {
    MatrixAccessor<double> macc = convert_bigMptr_to_MatrixAccessor(p_bigM);
    
    Rcpp::NumericVector res(macc.ncol());
    for (std::size_t j{0}; j < std::size_t(macc.ncol()); ++j) {
      for (std::size_t i{0}; i < std::size_t(macc.nrow()); ++i) {
        macc[j][i] += number;
      }
    }
  }
  
  
  void demean_bigMatrix() {
    
    MatrixAccessor<double> macc = convert_bigMptr_to_MatrixAccessor(p_bigM);
    Rcpp::NumericVector res = colMeans_of_bigMatrix();
    for (std::size_t j{0}; j < std::size_t(macc.ncol()); ++j) {
      for (std::size_t i{0}; i < std::size_t(macc.nrow()); ++i) {
        macc[j][i] -= res[j];
      }
    }
  }
  
  
  Rcpp::NumericVector colMeans_of_bigMatrix() {
    
    MatrixAccessor<double> macc = convert_bigMptr_to_MatrixAccessor(p_bigM);
    
    Rcpp::NumericVector res(macc.ncol());
    for (std::size_t j{0}; j < std::size_t(macc.ncol()); ++j) {
      for (std::size_t i{0}; i < std::size_t(macc.nrow()); ++i) {
        res[j] += macc[j][i];
      }
      res[j] /= std::size_t(macc.nrow());
    }
    
    return res;

  }
  
  
  Rcpp::NumericVector colStandardDeviation_of_bigMatrix(bool correction = true) {
    
    MatrixAccessor<double> macc = convert_bigMptr_to_MatrixAccessor(p_bigM);

    Rcpp::NumericVector big_ColMeans = colMeans_of_bigMatrix();    
    Rcpp::NumericVector res(macc.ncol());

    for (std::size_t j{0}; j < std::size_t(macc.ncol()); ++j) {
      for (std::size_t i{0}; i < std::size_t(macc.nrow()); ++i) {
        res[j] += std::pow((macc[j][i] - big_ColMeans[j]), 2);
      }
      if (correction) {
        res[j] /= (macc.nrow() - 1);        
      } else {
        res[j] /= macc.nrow();
      }
      res[j] = std::sqrt(res[j]);
    }

    return res;
  }
  
  
  void colStandardize_bigMatrix(bool correction = true) {
    
    MatrixAccessor<double> macc = convert_bigMptr_to_MatrixAccessor(p_bigM);
    Rcpp::NumericVector big_ColMeans = colMeans_of_bigMatrix();
    Rcpp::NumericVector big_ColStds = colStandardDeviation_of_bigMatrix(correction);
    
    for (std::size_t j{0}; j < std::size_t(macc.ncol()); ++j) {
      for (std::size_t i{0}; i < std::size_t(macc.nrow()); ++i) {
        macc[j][i] -= big_ColMeans[j];
        macc[j][i] /= big_ColStds[j];
      }
    }
    
  }
  
  
  void add_bigMatrix_to_new_bigMatrix(SEXP ptr_another_bigMat) {
    
    MatrixAccessor<double> macc1 = convert_bigMptr_to_MatrixAccessor(p_bigM);
    MatrixAccessor<double> macc2 = convert_bigMptr_to_MatrixAccessor(ptr_another_bigMat);
    
    if (macc1.ncol() != macc2.ncol() && macc1.nrow() != macc2.nrow()) {
      std::cerr << "Sorry matrix dimensions don't match!" << std::endl;
      return;
    }

    for (std::size_t j{0}; j < std::size_t(macc1.ncol()); ++j) {
      for (std::size_t i{0}; i < std::size_t(macc1.nrow()); ++i) {
            macc1[j][i] += macc2[j][i];
      }
    }
    
  }
  
  
  void subtract_bigMatrix_to_new_bigMatrix(SEXP ptr_another_bigMat) {
    
    MatrixAccessor<double> macc1 = convert_bigMptr_to_MatrixAccessor(p_bigM);
    MatrixAccessor<double> macc2 = convert_bigMptr_to_MatrixAccessor(ptr_another_bigMat);
    
    if (macc1.ncol() != macc2.ncol() && macc1.nrow() != macc2.nrow()) {
      std::cerr << "Sorry matrix dimensions don't match!" << std::endl;
      return;
    }

    for (std::size_t j{0}; j < std::size_t(macc1.ncol()); ++j) {
      for (std::size_t i{0}; i < std::size_t(macc1.nrow()); ++i) {
        macc1[j][i] -= macc2[j][i];
      }
    }
    
  }
  
  
  void multiply_bigMatrix_with_bigMatrix(SEXP ptr_another_bigMat, SEXP ptr_res_bigMat) {
    
    MatrixAccessor<double> macc1 = convert_bigMptr_to_MatrixAccessor(p_bigM);
    MatrixAccessor<double> macc2 = convert_bigMptr_to_MatrixAccessor(ptr_another_bigMat);
    MatrixAccessor<double> macc3 = convert_bigMptr_to_MatrixAccessor(ptr_res_bigMat);


    if (macc1.ncol() != macc2.nrow()) {
      std::cerr << "Sorry matrix dimensions don't match!" << std::endl;
      return;
    }
    if (macc3.nrow() != macc1.nrow() && macc3.ncol() != macc2.ncol()) {
      std::cerr << "Sorry matrix dimensions don't match!" << std::endl;
      return;
    }

    for (std::size_t n{0}; n < std::size_t(macc2.ncol()); ++n) {
      for (std::size_t i{0}; i < std::size_t(macc1.nrow()); ++i) {
        for (std::size_t j{0}; j < std::size_t(macc1.ncol()); ++j) {
          macc3[n][i] += macc1[j][i] * macc2[n][j];
        }
      }
    }
    
  }
  
  
  Rcpp::LogicalVector filter_bigMatrix_cols_for_same_column_number() {

    MatrixAccessor<double> macc = convert_bigMptr_to_MatrixAccessor(p_bigM);
    double first_val{};

    Rcpp::LogicalVector keep_idx(macc.ncol(), false);
    for(std::size_t j{0}; j < std::size_t(macc.ncol()); ++j) {
      first_val = macc[j][0];
      for(std::size_t i{1}; i < std::size_t(macc.nrow()); ++i) {
        if (macc[j][i] != first_val) {
          keep_idx[j] = true;
          break;
        }
      }
    }

    return keep_idx;
  }
  
  SEXP get_bigM_ptr() const {
    return p_bigM;
  }

private:
  // Attribute
  SEXP p_bigM;

  // class member methods
  MatrixAccessor<double> convert_bigMptr_to_MatrixAccessor(SEXP ptr_bigMat) {
    Rcpp::XPtr<BigMatrix> xpMat(ptr_bigMat);
    return MatrixAccessor<double> (*xpMat);    
  }

};


RCPP_MODULE(mod_External_bigMatrix_Admin) {

  Rcpp::class_<External_bigMatrix_Administration>("External_bigMatrix_Administration")
    .constructor<SEXP>("Creates new class obj.")
    .method("get_head_bigMatrix", &External_bigMatrix_Administration::get_head_bigMatrix, "Prints head of matrix.")
    .method("colSums_bigMatrix", &External_bigMatrix_Administration::colSums_bigMatrix, "Colum sums of big matrix.")
    .method("rowSums_bigMatrix", &External_bigMatrix_Administration::rowSums_bigMatrix, "Row sums of big matrix.")
    .method("colDifference_bigMatrix", &External_bigMatrix_Administration::colDifference_bigMatrix, "Colum difference of big matrix.")
    .method("multiply_scalar_with_bigMatrix", &External_bigMatrix_Administration::multiply_scalar_with_bigMatrix, "Multiply each matrix entry with a number.")
    .method("add_number_to_bigMatrix", &External_bigMatrix_Administration::add_number_to_bigMatrix, "Add number to each element of big matrix.")
    .method("colMeans_of_bigMatrix", &External_bigMatrix_Administration::colMeans_of_bigMatrix, "Determine column means of big matrix.")
    .method("demean_bigMatrix", &External_bigMatrix_Administration::demean_bigMatrix, "Subtract column means from big matrix.")
    .method("colStandardDeviation_of_bigMatrix", &External_bigMatrix_Administration::colStandardDeviation_of_bigMatrix, "Determine column standard deviation of big matrix.")
    .method("colStandardize_bigMatrix", &External_bigMatrix_Administration::colStandardize_bigMatrix, "Standardize big matrix columnwise.")
    .method("add_bigMatrix_to_new_bigMatrix", &External_bigMatrix_Administration::add_bigMatrix_to_new_bigMatrix, "Add another big matrix to class owned.")
    .method("subtract_bigMatrix_to_new_bigMatrix", &External_bigMatrix_Administration::subtract_bigMatrix_to_new_bigMatrix, "Subtract another big matrix from class owned.")
    .method("multiply_bigMatrix_with_bigMatrix", &External_bigMatrix_Administration::multiply_bigMatrix_with_bigMatrix, "Multiply a class member big matrix with another big matrix.")
    .method("filter_bigMatrix_cols_for_same_column_number", &External_bigMatrix_Administration::filter_bigMatrix_cols_for_same_column_number, "Return column indices of big matrix whose values are mutual exclusive with all other columns.")
    .method("get_bigM_ptr", &External_bigMatrix_Administration::get_bigM_ptr, "Retrieve pointer to big matrix.")
  ;
  
}

