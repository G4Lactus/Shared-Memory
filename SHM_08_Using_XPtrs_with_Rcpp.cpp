// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>


// Create a raw data pointer and return it back to R

/** Take A as a copy, create a new raw pointer to it, and return an external 
 *  pointer back to R.
 *  
 *  @return pMat, as SEXP, is an external pointer to the copy of A.
 */
// [[Rcpp::export]]
SEXP export_to_Xptr_SEXP(Rcpp::NumericMatrix A) {

  arma::mat* armaMat = new arma::mat(A.begin(), A.rows(), A.cols(), false, false);
  Rcpp::XPtr<arma::mat> pMat(armaMat);
  
  return pMat;
}


/** Take A as a copy, create a new raw pointer to it, and return an external
 *  pointer back to R.
 *  
 *  @return pmat, as XPtr<arma::mat>, is an external pointer of type xptr to the
 *          copy of A.
 */
// [[Rcpp::export]]
Rcpp::XPtr<arma::mat> export_Xptr(arma::mat A) {
  arma::mat* armaMat = new arma::mat(A.memptr(), A.n_rows, A.n_cols, false, false);
  Rcpp::XPtr<arma::mat> pmat(armaMat);
  return pmat;
}


/** Create a data object in C++, here a vector, and return it back to R as an
 *  XPtr.
 * 
 *  @return XPtr to the created data vector
 */
// [[Rcpp::export]]
Rcpp::XPtr<std::vector<double>> export_Cpp_vec_to_R() {
  std::vector<double>* vec_x = new std::vector<double>(10, 42);
  for (auto& elemX: *vec_x) {
    Rcpp::Rcout << elemX << std::endl;
  }
  return Rcpp::XPtr<std::vector<double>> (vec_x);
}


/**
 * Prints the content of a std::vector retrieved from an external pointer and
 * returns the raw data.
 * 
 * @return std::vector<double> of the vector's content
 */
// [[Rcpp::export]]
std::vector<double> retrieve_vec_from_xptr(Rcpp::XPtr<std::vector<double>>& vec) {
  for (auto& elemX: *vec) {
    Rcpp::Rcout << elemX << std::endl;
  }
  return *vec;
}


/** Retrieves the content of a matrix stored in an external pointer and returns
 *  it to R as a console output. Input is a general SEXP, and the function
 *  requires information about the stored matrix's dimension.
 *  
 *  @return void, just a console print out of the matrix's content
 */
// [[Rcpp::export]]
void retrieve_mat_from_SEXP(SEXP ptrA, std::size_t nr, std::size_t nc) {
  Rcpp::XPtr<arma::mat> xptrA(ptrA);
  xptrA->print();
}


/** Retrieves the content of a matrix stored in an external pointer and returns
 *  it to R as a console output. Input is a general SEXP, the xptr obj from R.
 *
 *  @return void, just a console print out of the matrix's content
 */
// [[Rcpp::export]]
void retrieve_mat_from_SEXP_no_paras(SEXP ptrA) {
  Rcpp::XPtr<arma::mat> xptrA(ptrA);
  xptrA->print();
}


/** Retrieves the content of a matrix stored in an external pointer SEXP and 
 *  returns it to R as a matrix.
 *
 *  @return arma::mat
 */
// [[Rcpp::export]]
arma::mat retrieve_mat_from_SEXPptr(SEXP ptrA) {
  Rcpp::XPtr<arma::mat> xptrA(ptrA);
  return *xptrA;
}


/** Retrieves the content of a matrix stored in an external pointer data type
 *  and returns it to R as a matrix. Input is XPtr<arma::mat>, not SEXP.
 *  If you input an SEXP the print statement shows zeros.
 * 
 * @return void, console output
 */
// [[Rcpp::export]]
void retrieve_mat_from_xptr(Rcpp::XPtr<arma::mat> xptrA) {
  xptrA->print();
}


/** Retrieves the content of a matrix stored in an external pointer data type
 *  and returns it to R as a matrix. Input is XPtr<arma::mat>, not SEXP.
 *  Furthermore, a new raw pointer is created from the input pointer and the
 *  data are printed from the newly created pointer.
 *  If you input an SEXP the print statement shows zeros.
 * 
 * @return
 */
// [[Rcpp::export]]
void retrieve_mat_from_xptr1(Rcpp::XPtr<arma::mat> xptrA) {
  arma::mat* A = new arma::mat((double*) xptrA->memptr(), xptrA->n_rows, xptrA->n_cols, false, false);
  A->print("copied matrix");
  delete A;
}

