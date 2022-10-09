#include <Rcpp.h>
// [[Rcpp::depends(BH, bigmemory)]]
#include <bigmemory/MatrixAccessor.hpp>



/** Determine vector of column sums from a bigmatrix of type bigmemory.
 *  In combination with boost (BH) and bigmemory accessible.
 * 
 * @return vector of colSums.
 */
// [[Rcpp::export]]
Rcpp::NumericVector colSum_bm(SEXP& bigMat) {
  
  Rcpp::XPtr<BigMatrix> xpMat(bigMat);
  MatrixAccessor<double> macc(*xpMat);

  std::size_t i, j, n = macc.nrow(), m = macc.ncol();
  
  // bigmatrix objs are column major oriented, i.e., [1][2], where 1 is the
  // column index, and 2 is the row index.
  Rcpp::NumericVector res(m);
  for (j = 0; j < m; ++j) {
    for (i = 0; i < n; ++i) {
      res[j] += macc[j][i];
    }
  }
  
  return res;
}



// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(bigstatsr, rmio)]]
#include <bigstatsr/BMAcc.h>

/** Determine vector of column sums from a big matrix of type FBM.
 *  In combination with bigstatsr and rmio accessible. The cpp11 plugin is only
 *  necessary if you use pre C++11 compiler in R. In 2022 usually no longer
 *  required.
 * 
 * @return vector of colSums.
 */
// [[Rcpp::export]]
Rcpp::NumericVector colSum_fbm(Rcpp::Environment& BM) {
  
  XPtr<FBM> xpBM = BM["address"]; // get the external pointer
  BMAcc<double> macc(xpBM);       // create data accessor
  
  std::size_t i, j, n = macc.nrow(), m = macc.ncol();
  Rcpp::NumericVector res(m); // vector of m zeros
  
  for (j = 0; j < m; ++j) {
    for (i = 0; i < n; ++i) {
      res[j] += macc(i, j);
    }
  }
  
  return res;
}



/** Determine colSums of a bigmatrix's submatrix.
 * 
 * @return vector of colSums from a submatrix.
 */
// [[Rcpp::export]]
Rcpp::NumericVector colSum_subfbm(Rcpp::Environment& BM,
                                    const Rcpp::IntegerVector& rowInd,
                                    const Rcpp::IntegerVector& colInd) {
  
  XPtr<FBM> xpBM = BM["address"];
  SubBMAcc<double> macc(xpBM, rowInd, colInd, 1); // sub-matrix accessor
  
  std::size_t i, j, n = macc.nrow(), m = macc.ncol();
  Rcpp::NumericVector res(m);
  
  for (j = 0; j < m; ++j) {
    for (i = 0; i < n; ++i) {
      res[j] += macc(i, j);
    }
  }
  
  return res;
}