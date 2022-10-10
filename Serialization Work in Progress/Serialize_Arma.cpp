// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(Rcereal)]]
#include <RcppArmadillo.h>
#include <cereal/cereal.hpp>
#include <cereal/archives/json.hpp>

namespace arma
{
// Wraps a particular column in a class with its own serialization function.
// This is necessary because cereal expects actual data to follow a size_tag, and can't
// serialize two size_tags back to back without creating a new node (entering a new serialization function).
//
// This wrapper serves the purpose of creating a new node in the JSON serializer and allows us to
// then serialize the size_tag, followed by the actual data
template <class T>
struct ColWrapper
{
  ColWrapper(T && m, int c, int nc) : mat(std::forward<T>(m)), col(c), n_cols(nc) {}
  T & mat;
  int col;
  int n_cols;
  
  template <class Archive>
  void save( Archive & ar ) const
  {
    ar( cereal::make_size_tag( mat.n_rows ) );
    for( auto iter = mat.begin_col(col), end = mat.end_col(col); iter != end; ++iter )
      ar( *iter );
  }
  
  template <class Archive>
  void load( Archive & ar )
  {
    cereal::size_type n_rows;
    
    // Test to see if we need to resize the data
    ar( cereal::make_size_tag( n_rows ) );
    if( mat.n_rows != n_rows )
      mat.resize( n_rows, n_cols );
    
    for( auto iter = mat.begin_col(col), end = mat.end_col(col); iter != end; ++iter )
      ar( *iter );
  }
};

// Convenience function to make a ColWrapper
template<class T> inline
  ColWrapper<T> make_col_wrapper(T && t, int c, int nc)
  {
    return {std::forward<T>(t), c, nc};
  }

template<class Archive, class eT, cereal::traits::EnableIf<cereal::traits::is_text_archive<Archive>::value> = cereal::traits::sfinae>
inline void save( Archive & ar, const Mat<eT>& m )
{
  // armadillo stored in column major order
  uword n_rows = m.n_rows;
  uword n_cols = m.n_cols;
  
  // First serialize a size_tag for the number of columns. This will make expect a dynamic
  // sized container, which it will output as a JSON array. In reality our container is not dynamic,
  // but we're going for readability here.
  ar( cereal::make_size_tag( n_cols ) );
  for( auto i = 0; i < n_cols; ++i )
    // a size_tag must be followed up with actual serializations that create nodes within the JSON serializer
    // so we cannot immediately make a size_tag for the number of rows. See ColWrapper for more details
    ar( make_col_wrapper(m, i, n_cols) );
}

template<class Archive, class eT, cereal::traits::EnableIf<cereal::traits::is_text_archive<Archive>::value> = cereal::traits::sfinae>
inline void load( Archive & ar, Mat<eT>& m )
{
  // We're doing essentially the same thing here, but loading the sizes and performing the resize for the matrix
  // within ColWrapper
  cereal::size_type n_rows;
  cereal::size_type n_cols;
  
  ar( cereal::make_size_tag( n_cols ) );
  for( auto i = 0; i < n_cols; ++i )
    ar( make_col_wrapper(m, i, n_cols) );
}
} // end namespace arma


// [[Rcpp::export]]
int main()
{
  std::stringstream ss;
  std::stringstream ss2;
  
  {
    arma::mat A = arma::randu<arma::mat>(4, 5);
    cereal::JSONOutputArchive ar(ss);
    ar( A );
  }
  std::cout << "Print out serialized matrix." << std::endl;
  std::cout << ss.str() << std::endl;
  
  
  {
    arma::mat A;
    cereal::JSONInputArchive ar(ss);
    ar( A );
    {
      cereal::JSONOutputArchive ar2(ss2);
      ar2( A );
    }
  }
  
  std::cout << "Print out deserialized matrix." << std::endl;
  std::cout << ss2.str() << std::endl;
  
  return 0;
}
