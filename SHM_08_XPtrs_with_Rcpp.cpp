// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// NOTE ON THIS TUTORIAL:
// This file demonstrates advanced and potentially dangerous memory management
// techniques. It is designed to illustrate *how* memory is handled at the
// R/C++ boundary, including common pitfalls. In production code, the safest
// patterns should always be preferred.


/**
 * @brief Creates a C++ matrix from an R matrix and returns it as an XPtr.
 * This function is the primary and recommended way to create a persistent
 * C++ object from R data.
 * 
 * @param A An R matrix, passed BY REFERENCE as an arma::mat view.
 * @param aux_mem Controls Armadillo's memory behavior.
 *        - `true` (SAFE & RECOMMENDED): Armadillo allocates its own memory and
 *          COPIES the data from the R matrix view. The XPtr "owns" a
 *          self-contained object.
 *        - `false` (DANGEROUS): The new C++ object points directly to R's
 *          memory. If the original R matrix is garbage-collected, this
 *          XPtr becomes a dangling pointer and will crash R.
 *          
 * @return An Rcpp::XPtr to the new C++ arma::mat.
 */
// [[Rcpp::export]]
Rcpp::XPtr<arma::mat> export_mat_to_XPtr_reference(const arma::mat& A, bool aux_mem) {
  arma::mat* armaMat = new arma::mat((double*) A.memptr(), A.n_rows, A.n_cols, aux_mem, false);
  Rcpp::XPtr<arma::mat> pmat(armaMat);
  return pmat;
}


/**
 * @brief Retrieves the data from an XPtr and returns it to R as a new matrix.
 * This is the standard way to get data out of an external pointer.
 * 
 * @param xptrA The external pointer object from R.
 * @return A new arma::mat containing a copy of the data.
 */
// [[Rcpp::export]]
arma::mat retrieve_mat_from_XPtr(Rcpp::XPtr<arma::mat> xptrA) {
  // Dereferencing the XPtr returns a reference to the C++ object. Rcpp then
  // creates a copy of this object to return to R.
  return *xptrA;
}


/**
 * @brief Modifies the data within a C++ object pointed to by an XPtr.
 * This demonstrates passing a pointer back to C++ to modify persistent data.
 * 
 * @param xptrA The external pointer object from R.
 * @return void. This function modifies its input in-place.
 */
// [[Rcpp::export]]
void work_with_xptr_stored_data(Rcpp::XPtr<arma::mat> xptrA) {
  Rcpp::Rcout << "Inside C++: Modifying the data held by the XPtr...\n";
  
  // Using the -> operator to access the methods of the pointed-to object.
  // Using the * operator to dereference and modify the data directly.
  *xptrA += 42;
  
  Rcpp::Rcout << "Modification complete.\n";
}


/**
 * @brief Creates a C++ native object (std::vector) on the heap and returns
 * an XPtr to it. This shows how to manage C++ objects that have no R equivalent.
 * 
 * @return An XPtr to a `std::vector<double>`.
 */
// [[Rcpp::export]]
Rcpp::XPtr<std::vector<double>> export_Cpp_vec_to_R() {
  // Create a new vector on the heap.
  std::vector<double>* vec_x = new std::vector<double>(10, 42.0);
  
  // Wrap it in an XPtr, which will handle memory deletion automatically.
  return Rcpp::XPtr<std::vector<double>>(vec_x);
}


/**
 * @brief Retrieves the data from a `std::vector` XPtr.
 * 
 * @param vec The external pointer to the C++ vector.
 * @return A copy of the `std::vector`.
 */
// [[Rcpp::export]]
std::vector<double> retrieve_vec_from_xptr(Rcpp::XPtr<std::vector<double>> vec) {
  return *vec;
}


// Functions below are for educational purposes to demonstrate pitfalls

/**
 * @brief DEMONSTRATION OF A GUARANTEED CRASH.
 * The `arma::mat A` signature creates a temporary copy. `aux_mem = false`
 * makes the new XPtr point to this temporary copy's memory. When the function
 * ends, the temporary copy is destroyed, creating a dangling pointer.
 *
 * This function is kept here for documentation but should not be used.
 */
// [[Rcpp::export]]
Rcpp::XPtr<arma::mat> export_mat_to_XPtr_guaranteed_crash(arma::mat A) {
  // This is extremely dangerous and is for demonstration only.
  arma::mat* armaMat = new arma::mat((double*)A.memptr(), A.n_rows, A.n_cols, false, false);
  Rcpp::XPtr<arma::mat> pmat(armaMat);
  return pmat;
}
