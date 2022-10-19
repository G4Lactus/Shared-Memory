#include <iostream>
#include <Rcpp.h>
// [[Rcpp::depends(BH, bigmemory)]]
#include <bigmemory/BigMatrix.h>
#include <bigmemory/MatrixAccessor.hpp>


/** Convert SEXP of bigmemory pointer to a matrix accessor stack obj.
 *  Note that the accessor is a pointer itself.
 */
MatrixAccessor<double> convert_bigMat_ptr_to_MatrixAccessor(SEXP ptr_bigMat) {
  Rcpp::XPtr<BigMatrix> xpMat(ptr_bigMat);
  return MatrixAccessor<double> (*xpMat); 
}


/** Convert SEXP of bigmemory pointer to a matrix accessor pointer, heap obj.
 *  Note that the accessor is a pointer itself. But now we create just a pointer
 *  to the obj.
 */
MatrixAccessor<double>* convert_bigMat_ptr_to_MatrixAccessor_ptr(SEXP ptr_bigMat) {
  Rcpp::XPtr<BigMatrix> xpMat(ptr_bigMat);
  MatrixAccessor<double>* macc_ptr = new MatrixAccessor<double>((double*)xpMat->matrix(), xpMat->nrow(), xpMat->ncol());
  return macc_ptr;
}


/** The following function operates based on two bigmemory pointers and writes
 *  based on the 1st the content to the second and augments the column dimension
 *  of the 1st based on random numbers in amount of the same column dimensions.
 *  After the second matrix is written, the first matrix is overwritten based
 *  on the value 42. The 2nd big matrix's row dimension must equal the 1st's,
 *  and its column dimension must be twice the dimension of the 1st.
 */
// [[Rcpp::export]]
void create_extended_X(const SEXP& ptr_bigMat, const SEXP& ptr_exbigMat) {
  
  // stack objs
  MatrixAccessor<double> macc1 = convert_bigMat_ptr_to_MatrixAccessor(ptr_bigMat);
  MatrixAccessor<double> macc2 = convert_bigMat_ptr_to_MatrixAccessor(ptr_exbigMat);
  if (macc2.nrow() != macc1.nrow() && macc2.ncol() != macc1.ncol()) {
    std::cerr << "Sorry matrix dimensions don't match!"
              << " Please enter a second matrix whose dimensions are as follow:\n"
              << "nrow2 = nrow1, ncol2 = 2*ncol1" << std::endl;
  }
  
  // heap objs
  MatrixAccessor<double>* macc3 = convert_bigMat_ptr_to_MatrixAccessor_ptr(ptr_bigMat);
  
  
  Rcpp::Rcout << "Ptr retrieved ncols: " << macc3->ncol() << std::endl;
  Rcpp::Rcout << "Ptr retrieved nrows: " << macc3->nrow() << std::endl;
  
  // original data
  for (std::size_t j{0}; j < std::size_t(macc1.ncol()); ++j) {
    for (std::size_t i{0}; i < std::size_t(macc1.nrow()); ++i) {
      macc2[j][i] = macc1[j][i];
    }
  }
  
  // add dummy variables
  for (std::size_t j{std::size_t(macc1.ncol())}; j < std::size_t(macc2.ncol()); ++j) {
    for (std::size_t i{0}; i < std::size_t(macc1.nrow()); ++i) {
      macc2[j][i] = R::norm_rand();
    }
  }
  
  // overwrite original data
  for (std::size_t j{0}; j < std::size_t(macc3->ncol()); ++j) {
    for (std::size_t i{0}; i < std::size_t(macc3->nrow()); ++i) {
      Rcpp::Rcout << "Memory address: "<< macc3[j][i] << std::endl;
      Rcpp::Rcout << "Old Value: " << (*macc3)[j][i] << std::endl;
      (*macc3)[j][i] = 42;
      Rcpp::Rcout << "New value: " << (*macc3)[j][i] << std::endl;
    }
  }
  
  delete macc3; // release raw pointer
  
}


/*** R
library(bigmemory)
bigM <- bigmemory::as.big.matrix(x = matrix(1:12, nrow = 3, ncol = 4), type = "double")
ex_bigM <- bigmemory::big.matrix(nrow = nrow(bigM), ncol = 2*ncol(bigM),
                                 type = "double", init = 0)

create_extended_X(bigM@address, ex_bigM@address)
*/



/** Operation based on stack objs.
 */ 
// [[Rcpp::export]]
void create_extended_X_stack_macc(const SEXP& ptr_bigMat, const SEXP& ptr_exbigMat) {
  
  // stack objs
  MatrixAccessor<double> macc1 = convert_bigMat_ptr_to_MatrixAccessor(ptr_bigMat);
  MatrixAccessor<double> macc2 = convert_bigMat_ptr_to_MatrixAccessor(ptr_exbigMat);
  if (macc2.nrow() != macc1.nrow() && macc2.ncol() != macc1.ncol()) {
    std::cerr << "Sorry matrix dimensions don't match!"
              << " Please enter a second matrix whose dimensions are as follow:\n"
              << "nrow2 = nrow1, ncol2 = 2*ncol1" << std::endl;
  }
  
  
  Rcpp::Rcout << "Size of macc stack objs: " << std::endl;
  Rcpp::Rcout << sizeof(macc1) << std::endl;
  Rcpp::Rcout << sizeof(macc2) << std::endl;  
  
  // original data
  for (std::size_t j{0}; j < std::size_t(macc1.ncol()); ++j) {
    for (std::size_t i{0}; i < std::size_t(macc1.nrow()); ++i) {
      macc2[j][i] = macc1[j][i];
    }
  }
  
  // add dummy variables
  for (std::size_t j{std::size_t(macc1.ncol())}; j < std::size_t(macc2.ncol()); ++j) {
    for (std::size_t i{0}; i < std::size_t(macc1.nrow()); ++i) {
      macc2[j][i] = R::norm_rand();
    }
  }
}


/*** R
library(bigmemory)
bigM1 <- bigmemory::as.big.matrix(x = matrix(1:1200000, nrow = 3, ncol = 4e5), type = "double")
ex_bigM1 <- bigmemory::big.matrix(nrow = nrow(bigM1), ncol = 2*ncol(bigM1),
                                  type = "double", init = 0)

create_extended_X(bigM1@address, ex_bigM1@address)
*/


/** Operation based on heap objs.
 */ 
// [[Rcpp::export]]
void create_extended_X_ptr_heap_macc(SEXP& ptr_bigMat, const SEXP& ptr_exbigMat) {
  
  // heap objs
  MatrixAccessor<double>* macc1 = convert_bigMat_ptr_to_MatrixAccessor_ptr(ptr_bigMat);
  MatrixAccessor<double>* macc2 = convert_bigMat_ptr_to_MatrixAccessor_ptr(ptr_exbigMat);
  if (macc2->nrow() != macc1->nrow() && macc2->ncol() != macc1->ncol()) {
    std::cerr << "Sorry matrix dimensions don't match!"
              << " Please enter a second matrix whose dimensions are as follow:\n"
              << "nrow2 = nrow1, ncol2 = 2*ncol1" << std::endl;
  }
  
  
  Rcpp::Rcout << "Size of macc heap objs: " << std::endl;  
  Rcpp::Rcout << sizeof(macc1) << std::endl;
  Rcpp::Rcout << sizeof(macc2) << std::endl;  
  
  // original data
  for (std::size_t j{0}; j < std::size_t(macc1->ncol()); ++j) {
    for (std::size_t i{0}; i < std::size_t(macc1->nrow()); ++i) {
      (*macc2)[j][i] = (*macc1)[j][i];
    }
  }
  
  // add dummy variables
  for (std::size_t j{std::size_t(macc1->ncol())}; j < std::size_t(macc2->ncol()); ++j) {
    for (std::size_t i{0}; i < std::size_t(macc1->nrow()); ++i) {
      (*macc2)[j][i] = R::norm_rand();
    }
  }
  
  // release raw pointers
  delete macc1;
  delete macc2;
}


/*** R
bigM2 <- bigmemory::as.big.matrix(x = matrix(1:1200000, nrow = 3, ncol = 4e5), type = "double")
ex_bigM2 <- bigmemory::big.matrix(nrow = nrow(bigM2), ncol = 2*ncol(bigM2),
                                  type = "double", init = 0)

create_extended_X_ptr(bigM2@address, ex_bigM2@address)
*/


