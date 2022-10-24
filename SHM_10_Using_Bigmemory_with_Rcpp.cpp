#include <bigmemory/BigMatrix.h>
#include <bigmemory/MatrixAccessor.hpp>
// [[Rcpp::depends(RcppArmadillo, BH, bigmemory)]]
#include <RcppArmadillo.h>


// Example 1: big column sum for column X
// ----------------------------------------------------------------------------
// Inspiration:
// ------------
// https://gallery.rcpp.org/articles/using-bigmemory-with-rcpp/
// bigmemory matrices support the following data types:
// - char, short, integer, and double
// To get general functionality we can use a templated function.
// ----------------------------------------------------------------------------
template <typename T>
arma::Col<T> bigArma_colSums(const arma::Mat<T>& bigMat, Rcpp::IntegerVector colX) {
  arma::uvec colidx = Rcpp::as<arma::uvec>(colX) - 1;
  return sum(bigMat.cols(colidx));
}

// Templated functions/classes can't be exported to R. Therefore we have to
// define an accessor and return function.

// [[Rcpp::export]]
Rcpp::NumericVector big_colSums(SEXP ptr_bigMat, Rcpp::IntegerVector colX) {
  
  // first crucial step to receive access to ptr_bigMat, which is no XPtr
  Rcpp::XPtr<BigMatrix> xpMat(ptr_bigMat);
  
  if (Rcpp::is_true(Rcpp::any(colX < 1)) || Rcpp::is_true(Rcpp::any(colX > xpMat->ncol())) ) {
    throw std::out_of_range("Some of requested columns are outside of the matrix!");
  }
  
  std::size_t type = xpMat -> matrix_type();
  switch(type) {
    case 1:
    {
      // arma matrix constructor for pointer
      arma::Col<char> colSums = bigArma_colSums(
        arma::Mat<char>((char*)xpMat->matrix(), xpMat->nrow(), xpMat->ncol(), false),
        colX
      );
      return Rcpp::NumericVector(colSums.begin(), colSums.end());
    }
  
    case 2:
    {
      arma::Col<short> colSums = bigArma_colSums(
        arma::Mat<short>((short*) xpMat -> matrix(), xpMat->nrow(), xpMat->ncol(), false),
        colX
      );
      return Rcpp::NumericVector(colSums.begin(), colSums.end());
    }
  
    case 4:
    {
      arma::Col<int> colSums = bigArma_colSums(
        arma::Mat<int>((int*) xpMat->matrix(), xpMat->nrow(), xpMat->ncol(), false),
        colX
      );
      return Rcpp::NumericVector(colSums.begin(), colSums.end());
    }
  
    case 8:
    {
      arma::Col<double> colSums = bigArma_colSums(
        arma::Mat<double>((double*)xpMat->matrix(), xpMat->nrow(), xpMat->ncol(), false),
        colX
      );
      return Rcpp::NumericVector(colSums.begin(), colSums.end());
    }
  
    default:
      throw Rcpp::exception("Undefined data type for provided big.matrix.");
  }
}
// ----------------------------------------------------------------------------



// Example 2: Row sums
// ---------------------------------------------------------------------------
// https://stackoverflow.com/questions/24687392/computing-row-sums-of-a-big-matrix-in-r
// 
// NOTE: the warning message regarding the std::auto_ptr stems from the MatrixAccessor obj
// ----------------------------------------------------------------------------
template <typename T>
Rcpp::NumericVector big_rowSums(Rcpp::XPtr<BigMatrix> ptr_BigMat, MatrixAccessor<T> macc) {
  Rcpp::NumericVector rowSums(ptr_BigMat->nrow(), 0.0);
  Rcpp::NumericVector value(1);
  
  for (std::size_t j{0}; j < std::size_t(ptr_BigMat->ncol()); ++j) {
    for (std::size_t i{0}; i < std::size_t(ptr_BigMat->nrow()); ++i) {
      value = macc[j][i];
      if (Rcpp::all(!Rcpp::is_na(value))) {
        rowSums[i] += value[0];
      }
    }
  }
  return rowSums;
}

// [[Rcpp::export]]
Rcpp::NumericVector big_rowSums(SEXP ptr_BigMat) {
  Rcpp::XPtr<BigMatrix> xptr_Mat(ptr_BigMat);
  
  switch(xptr_Mat->matrix_type()) {
    case 1:
      return big_rowSums(xptr_Mat, MatrixAccessor<char>(*xptr_Mat));
    case 2:
      return big_rowSums(xptr_Mat, MatrixAccessor<short>(*xptr_Mat));
    case 4:
      return big_rowSums(xptr_Mat, MatrixAccessor<int>(*xptr_Mat));
    case 6:
      return big_rowSums(xptr_Mat, MatrixAccessor<float>(*xptr_Mat));
    case 8:
      return big_rowSums(xptr_Mat, MatrixAccessor<double>(*xptr_Mat));
    default:
      throw Rcpp::exception("Unknown type detected for big.matrix obj!");
  }
  
}
// ---------------------------------------------------------------------------



// Example 3: calculate total sum of a matrix
// ---------------------------------------------------------------------------
// Total sum of a double type big matrix
// [[Rcpp::export]]
double sum_bigMat(SEXP pBigMat, int n_row, int n_col){
  Rcpp::XPtr<BigMatrix> xpMat(pBigMat);
  MatrixAccessor<double> mat(*xpMat);
  double sum = 0;
  
  for(int colX=0; colX<n_col; colX++){ 
    for(int rowX=0; rowX<n_row; rowX++){
      sum += mat[colX][rowX];
    }
  }
  return(sum);
}
// ---------------------------------------------------------------------------



// Example 4: cast big matrix to armadillo matrix in C++ and print out
// ---------------------------------------------------------------------------
// [[Rcpp::export]]
void armacast(SEXP ptr_Data, bool stack_obj) {
  Rcpp::XPtr<BigMatrix> xptr_Data(ptr_Data);
  
  if (xptr_Data->matrix_type() == 8) {
    if (stack_obj) {
      // constructor new stack obj for bigmatrix input
      arma::mat M((double*)xptr_Data->matrix(), xptr_Data->nrow(), xptr_Data->ncol(),
                  false, false);
      M.print("Arma matrix M");
    } else {
      // constructor new heap obj for bigmatrix input
      arma::mat* M = new arma::mat((double*)xptr_Data->matrix(), xptr_Data->nrow(),
                                   xptr_Data->ncol(), false, false);
      M->print();
      delete M; // avoid memory leak
    }

  } else {
    throw Rcpp::exception("Unknown type of big.matrix detected! Aborting.");
  }
}
// ---------------------------------------------------------------------------


// Example 5: another column sum return
// --------------------------------------------------------------------------
/** Determine vector of column sums from a bigmatrix of type bigmemory.
 *  In combination with boost (BH) and bigmemory accessible.
 *
 * @return vector of colSums.
 *
 * Alternative version:
 * // https://www.r-bloggers.com/2013/03/using-bigmemory-with-rcpp/
 *
 */
// [[Rcpp::export]]
Rcpp::NumericVector colSum_bm(Rcpp::XPtr<BigMatrix> ptr_BigMat) {
  
  // Create the matrix accessor so we can get the elements of the matrix
  MatrixAccessor<double> macc(*ptr_BigMat);
  
  // Create the vector we'll store the column sumns in
  Rcpp::NumericVector colSums(ptr_BigMat -> ncol());
  for (std::size_t i{0}; i < std::size_t(ptr_BigMat -> ncol()); ++i) {
    colSums[i] = std::accumulate(macc[i], macc[i] + ptr_BigMat->nrow(), 0.0);
  }
  
  return colSums;
}
// --------------------------------------------------------------------------



// Example 6: Change all non.zero numbers to a constant
// --------------------------------------------------------------------------
// Inspiration:
// https://stackoverflow.com/questions/49960165/change-all-nonzero-numbers-in-big-matrix-to-1
// ----------------------------------------------------------
template <typename T>
void nz_to_const(Rcpp::XPtr<BigMatrix> ptr_BigMat, MatrixAccessor<T> macc, T new_val) {

  for (std::size_t j{0}; j < std::size_t(macc.ncol()); ++j) {
    for (std::size_t i{0}; i < std::size_t(macc.nrow()); ++i) {
      if (macc[j][i] != 0) {
        macc[j][i] = new_val;
      }
    }
  }
  
}

// [[Rcpp::export]]
void big_nz_to_const(SEXP ptr_BigMat, SEXP new_val) {
  Rcpp::XPtr<BigMatrix> xptr_Mat(ptr_BigMat);
  
  switch(xptr_Mat->matrix_type()) {
  case 1:
    return nz_to_const(xptr_Mat, MatrixAccessor<char>(*xptr_Mat), Rcpp::as<char>(new_val));
  case 2:
    return nz_to_const(xptr_Mat, MatrixAccessor<short>(*xptr_Mat), Rcpp::as<short>(new_val));
  case 4:
    return nz_to_const(xptr_Mat, MatrixAccessor<int>(*xptr_Mat), Rcpp::as<int>(new_val));
  case 6:
    return nz_to_const(xptr_Mat, MatrixAccessor<float>(*xptr_Mat), Rcpp::as<float>(new_val));
  case 8:
    return nz_to_const(xptr_Mat, MatrixAccessor<double>(*xptr_Mat), Rcpp::as<double>(new_val));
  default:
    throw Rcpp::exception("Unknown type detected for big.matrix obj!");
  }
  
}
// --------------------------------------------------------------------------



// Example 7: extracting a column with NA's from a bigmemory obj in Rcpp
// --------------------------------------------------------------------------
// Logic for extracting a column with NA's from a Big Matrix object
// 
// https://stackoverflow.com/questions/38569321/extracting-a-column-with-nas-from-a-bigmemory-object-in-rcpp
// -----------------------------------
#include <bigmemory/isna.hpp>
// ----------------------------
template <typename T>
Rcpp::LogicalVector get_filterColumn_logic(Rcpp::XPtr<BigMatrix> pMat, MatrixAccessor<T> mat, int cn) {
  Rcpp::LogicalVector nv(pMat->nrow());
  for(int i = 0; i < pMat->nrow(); i++) {
    if(isna(mat[cn][i])) {
      nv[i] = true;
    } else {
      nv[i] = false;
    }
  }

  return nv;
}
// ----------------------------


//' Extract Column from a Big Matrix.
//'
//' @param pBigMat A bigmemory object address.
//' @param colNum Column Number to extract. Indexing starts from zero.
//' @export
// [[Rcpp::export]]
Rcpp::LogicalVector get_filterColumn(SEXP pBigMat, int colNum) {
  Rcpp::XPtr<BigMatrix> xpMat(pBigMat);
  
  switch(xpMat->matrix_type()) {
  case 1: return get_filterColumn_logic(xpMat, MatrixAccessor<char>(*xpMat), colNum);
  case 2: return get_filterColumn_logic(xpMat, MatrixAccessor<short>(*xpMat), colNum);
  case 4: return get_filterColumn_logic(xpMat, MatrixAccessor<int>(*xpMat), colNum);
  case 6: return get_filterColumn_logic(xpMat, MatrixAccessor<float>(*xpMat), colNum);
  case 8: return get_filterColumn_logic(xpMat, MatrixAccessor<double>(*xpMat), colNum);
  default: throw Rcpp::exception("Unknown type detected for big.matrix object!");
  }
}


/**
 * Imputation of "NA" values for "1" in a big 0, 2 NA matrix.
 */
// [[Rcpp::export]]
void bigMat_impute_NA(SEXP pBigMat) {
  
  
  // Create the external bigmatrix pointer and indiciate matrix accessor
  Rcpp::XPtr<BigMatrix> xpMat(pBigMat);
  MatrixAccessor<double> mat = (*xpMat);
  
  // Iterate over the elements in a matrix and when NA is found, substitute for "1"
  for(int i=0; i < xpMat->ncol(); i++){
    for(int j=0; j < xpMat->nrow(); j++){
      if(mat[i][j] == NA_INTEGER) {
        mat[i][j] = 1;
      }
    }
  }
  
  return;
}


// [[Rcpp::export]]
Rcpp::LogicalVector inds_to_keep(SEXP bm_addr) {

  Rcpp::XPtr<BigMatrix> xptr(bm_addr);
  MatrixAccessor<double> macc(*xptr);

  std::size_t n = macc.nrow();
  std::size_t m = macc.ncol();

  double first_val{};

  Rcpp::LogicalVector keep_idx(m, false);

  for(std::size_t j{0}; j < m; ++j) {
    first_val = macc[j][0];
    for(std::size_t i{1}; i < n; ++i) {
      if (macc[j][i] != first_val) {
        keep_idx[j] = true;
        break;
      }
    }
  }

  return keep_idx;
}
// ----------------------------------------------------------------------------



// Example 8:
// ----------------------------------------------------------------------------
// general transpose
// https://stackoverflow.com/questions/42161237/using-rcpparmadillo-with-bigmemory-returning-a-bigmatrix
// [[Rcpp::export]]
void transpose_bigMat(SEXP pBigMat,SEXP pBigMat_t) {
    
  Rcpp::XPtr<BigMatrix> xpMat(pBigMat);
  Rcpp::XPtr<BigMatrix> xpMat_t(pBigMat_t);
    
  arma::Mat<double> aBigMat((double *)xpMat->matrix(), xpMat->nrow(), xpMat->ncol(), false);
  arma::Mat<double> tBigMat(aBigMat.t());
    
  MatrixAccessor<double> Am(*xpMat_t);
  
  for (int i = 0; i < xpMat->nrow(); i++){
    std::memcpy(Am[i], tBigMat.colptr(i) , xpMat->ncol()*sizeof(double));
  }
    
  return;
}
// ----------------------------------------------------------------------------



// Example 9:
// ----------------------------------------------------------------------------
// [[Rcpp::export]]
Rcpp::NumericVector prod_BigMat_with_vector(Rcpp::XPtr<BigMatrix> bM_ptr, const Rcpp::NumericVector& x) {
  MatrixAccessor<char> macc(*bM_ptr);
  Rcpp::NumericVector res(bM_ptr->nrow());

  for (std::size_t colX{0}; colX < std::size_t(bM_ptr->ncol()); ++colX) {
    for (std::size_t rowX{0}; rowX < std::size_t(bM_ptr->nrow()); ++rowX) {
      res[rowX] += macc[colX][rowX] * x[colX];
    }
  }

  return res;
}
// ----------------------------------------------------------------------------



// Example 10: Operate on the big matrix obj
// ----------------------------------------------------------------------------
// [[Rcpp::export]]
void local_changes_on_bigMat(SEXP A) {
  Rcpp::XPtr<BigMatrix> bigMat(A);
  MatrixAccessor<int> Am(*bigMat);

  for (int j = 0; j < bigMat->ncol(); ++j) {
    for (int i = 1; i < bigMat->nrow(); ++i) {
      Am[j][i] += Am[j][i-1];
    }
  }

  return;
}
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Generic Implementation of get diagonal for a squre matrix
// https://stackoverflow.com/questions/24622918/initialize-a-variable-with-different-type-based-on-a-switch-statement
// -------------------------
template <typename T>
Rcpp::NumericVector get_bigMat_diag(Rcpp::XPtr<BigMatrix> xpMat, MatrixAccessor<T> mat) {
  Rcpp::NumericVector diag(xpMat->nrow());
  for (unsigned int i = 0; i < xpMat->nrow(); i++) {
    diag[i] = mat[i][i];
  }
  return diag;
}


// Dispatch code
// [[Rcpp::export]]
Rcpp::NumericVector get_bigMat_diag(SEXP pMat) {
  Rcpp::XPtr<BigMatrix> xpMat(pMat);
  switch(xpMat->matrix_type()) {
  case 1:
    return get_bigMat_diag(xpMat, MatrixAccessor<char>(*xpMat));
  case 2:
    return get_bigMat_diag(xpMat, MatrixAccessor<short>(*xpMat));
  case 4:
    return get_bigMat_diag(xpMat, MatrixAccessor<int>(*xpMat));
  case 8:
    return get_bigMat_diag(xpMat, MatrixAccessor<double>(*xpMat));
  default:
    // This should be impossible to reach, but shuts up the compiler
    throw Rcpp::exception("Unknown type of big.matrix detected! Aborting.");
  }
}
// ----------------------------------------------------------------------------

