// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>


// NOTE: manual memory management is a very dangerous operation, as soon as
// your references are lost and you try to access the created objs in R, the
// session crashes.
// Note that if you hand to XPtr another pointer, XPtrs own finalizer will 
// release the pointer as soon as your XPtr obj is gone.


/** Take A as a Rcpp::NumericMatrix copy and create a new raw pointer to it.
 *  From it return an external pointer back to R as SEXP obj
 *  The parameter aux_mem controls if the new arma::mat obj pointer uses
 *  its own memory or that of the copy of A.
 *  In both cases, we can retrieve the original matrix back.
 *  
 *  @return pMat, as SEXP, is an external pointer to the copy of A.
 */
// [[Rcpp::export]]
SEXP export_mat_to_Xptr_SEXP(Rcpp::NumericMatrix A, bool aux_mem) {
  arma::mat* armaMat = new arma::mat(A.begin(), A.rows(), A.cols(), aux_mem, false);
  Rcpp::XPtr<arma::mat> pMat(armaMat);
  return pMat;
}


/** Take A as a arma::mat copy and create a new raw pointer to it. From it
 *  return an external pointer back to R as XPtr<arma::mat> obj.
 *  NOTE: as aux_mem is false, we can't restore A anymore, because as we leave
 *        the function no connection to the copy of A exists anymore. 
 *  
 *  @return pmat, as XPtr<arma::mat>, is an external pointer to the copy of A.
 */
// [[Rcpp::export]]
Rcpp::XPtr<arma::mat> export_mat_to_XPtr_copy(arma::mat A, bool aux_mem) {
  arma::mat* armaMat = new arma::mat((double*)A.memptr(), A.n_rows, A.n_cols, aux_mem, false);
  armaMat->print();
  Rcpp::XPtr<arma::mat> pmat(armaMat);
  return pmat;
}


/** Take A as a arma::mat reference and create a new raw pointer to it.
 *  From it return an external pointer back to R as XPtr<arma::mat> obj.
 *  NOTE: as aux_mem is false, we can still restore A because as we are passing
 *        a reference to the original quantity.
 *          
 *  @return pmat, as XPtr<arma::mat>.
 */
// [[Rcpp::export]]
Rcpp::XPtr<arma::mat> export_mat_to_XPtr_reference(arma::mat& A, bool aux_mem) {
  arma::mat* armaMat = new arma::mat((double*)A.memptr(), A.n_rows, A.n_cols, aux_mem, false);
  Rcpp::XPtr<arma::mat> pmat(armaMat);
  return pmat;
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


/** Retrieves the content of a matrix stored in an external pointer SEXP and 
 *  returns it to R as a matrix.
 *
 *  @return arma::mat
 */
// [[Rcpp::export]]
arma::mat retrieve_mat_from_XPtr(Rcpp::XPtr<arma::mat> xptrA) {
  return *xptrA;
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
 * Retrieves the content of a std::vector from an external pointer.
 * 
 * @return std::vector<double> of the vector's content
 */
// [[Rcpp::export]]
std::vector<double> retrieve_vec_from_xptr(Rcpp::XPtr<std::vector<double>>& vec) {
  return *vec;
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

/** Retrieves the content of a matrix stored in an external pointer and allows
 *  to continue working with the data at the C++ backend.
 *  NOTE: making changes on the data, changes the data stored in R as well.
 */
// [[Rcpp::export]]
void work_with_xptr_stored_data(Rcpp::XPtr<arma::mat> xptrA, bool aux_mem) {
  arma::mat* B = new arma::mat((double*) xptrA->memptr(), xptrA->n_rows, xptrA->n_cols, aux_mem, false);
  Rcpp::Rcout << "Print B:\n";
  B->print();
  
  Rcpp::Rcout << "Print A:\n";
  xptrA->print();
  Rcpp::Rcout << std::endl;
  *xptrA += 42;
  Rcpp::Rcout << "Print A:\n";
  xptrA->print();
  Rcpp::Rcout << std::endl;
  *xptrA *= xptrA->t();
  Rcpp::Rcout << "Print A:\n";
  xptrA->print();
  Rcpp::Rcout << std::endl;
  
  // NOTE: After multiplying A with its transpose, B strict policy (false)
  //       comes into play, due to a change in size of xptrA's value.
  //       If strict is set to false, the matrix will use the auxiliary memory
  //       until a size change or an aliasing event.
  Rcpp::Rcout << "Print B:\n";
  B->print();
  
  Rcpp::Rcout << "Print A:\n";
  xptrA->print();
  delete B; // B is a raw pointer and if we don't release a memory leak occurs
}
