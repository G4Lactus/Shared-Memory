#include <iostream>
#include <sstream>
// [[Rcpp::depends(RcppArmadillo, BH)]]
#include <boost/serialization/serialization.hpp>
#include <boost/serialization/nvp.hpp>
#include <boost/serialization/array.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>

//! Add a serialization operator.
template<typename Archive>
void serialize(Archive& ar, const unsigned int version);

#include <RcppArmadillo/Mat_proto.h>
#ifndef RcppArmadillo__RcppArmadilloForward__h
#define RcppArmadillo__RcppArmadilloForward__h

#include <RcppCommon.h>
#include <Rconfig.h>
#include <RcppArmadilloConfig.h>

// Custom Mat extension that combines MLPACK with RcppArmadillo's Mat extensions:
#define ARMA_EXTRA_MAT_PROTO mat_extra_bones.hpp
#define ARMA_EXTRA_MAT_MEAT  mat_extra_meat.hpp

// Everything else the same:
#define ARMA_EXTRA_COL_PROTO RcppArmadillo/Col_proto.h
#define ARMA_EXTRA_COL_MEAT  RcppArmadillo/Col_meat.h
#define ARMA_EXTRA_ROW_PROTO RcppArmadillo/Row_proto.h
#define ARMA_EXTRA_ROW_MEAT  RcppArmadillo/Row_meat.h
#define ARMA_RNG_ALT         RcppArmadillo/Alt_R_RNG.h
#include <armadillo>
/* forward declarations */
namespace Rcpp {
/* support for wrap */
template <typename T> SEXP wrap ( const arma::Mat<T>& ) ;
template <typename T> SEXP wrap ( const arma::Row<T>& ) ;
template <typename T> SEXP wrap ( const arma::Col<T>& ) ;
template <typename T> SEXP wrap ( const arma::field<T>& ) ;
template <typename T> SEXP wrap ( const arma::Cube<T>& ) ;
template <typename T> SEXP wrap ( const arma::subview<T>& ) ;
template <typename T> SEXP wrap ( const arma::SpMat<T>& ) ;

template <typename T1, typename T2, typename glue_type> 
SEXP wrap(const arma::Glue<T1, T2, glue_type>& X ) ;

template <typename T1, typename op_type>
SEXP wrap(const arma::Op<T1, op_type>& X ) ;

template <typename T1, typename T2, typename glue_type> 
SEXP wrap(const arma::eGlue<T1, T2, glue_type>& X ) ;

template <typename T1, typename op_type>
SEXP wrap(const arma::eOp<T1, op_type>& X ) ;

template <typename T1, typename op_type>
SEXP wrap(const arma::OpCube<T1,op_type>& X ) ;

template <typename T1, typename T2, typename glue_type>
SEXP wrap(const arma::GlueCube<T1,T2,glue_type>& X ) ;

template <typename T1, typename op_type>
SEXP wrap(const arma::eOpCube<T1,op_type>& X ) ;

template <typename T1, typename T2, typename glue_type>
SEXP wrap(const arma::eGlueCube<T1,T2,glue_type>& X ) ;

template<typename out_eT, typename T1, typename op_type>
SEXP wrap( const arma::mtOp<out_eT,T1,op_type>& X ) ;

template<typename out_eT, typename T1, typename T2, typename glue_type>
SEXP wrap( const arma::mtGlue<out_eT,T1,T2,glue_type>& X );

template <typename eT, typename gen_type>
SEXP wrap( const arma::Gen<eT,gen_type>& X) ;

template<typename eT, typename gen_type>
SEXP wrap( const arma::GenCube<eT,gen_type>& X) ;

namespace traits {

/* support for as */
template <typename T> class Exporter< arma::Mat<T> > ;
template <typename T> class Exporter< arma::Row<T> > ;
template <typename T> class Exporter< arma::Col<T> > ;
template <typename T> class Exporter< arma::SpMat<T> > ;

template <typename T> class Exporter< arma::field<T> > ;
// template <typename T> class Exporter< arma::Cube<T> > ;

} // namespace traits 

template <typename T> class ConstReferenceInputParameter< arma::Mat<T> > ;
template <typename T> class ReferenceInputParameter< arma::Mat<T> > ;
template <typename T> class ConstInputParameter< arma::Mat<T> > ;

template <typename T> class ConstReferenceInputParameter< arma::Col<T> > ;
template <typename T> class ReferenceInputParameter< arma::Col<T> > ;
template <typename T> class ConstInputParameter< arma::Col<T> > ;

template <typename T> class ConstReferenceInputParameter< arma::Row<T> > ;
template <typename T> class ReferenceInputParameter< arma::Row<T> > ;
template <typename T> class ConstInputParameter< arma::Row<T> > ;

}

#endif
#include <RcppArmadillo.h>


// [[Rcpp::export]]
Rcpp::RawVector test_serialization(arma::Mat<double> m) {
  std::stringstream ss;
  // save data to archive
  {
    boost::archive::binary_oarchive oarchive(ss); // create an output archive
    oarchive & m; // write class instance to archive
    // archive and stream closed when destructors are called
  }
  ss.seekg(0, ss.end);
  Rcpp::RawVector retval(ss.tellg());
  ss.seekg(0, ss.beg);
  ss.read(reinterpret_cast<char*>(&retval[0]), retval.size());
  return retval;
}