// [[Rcpp::depends(Rcereal)]]
#include <iostream>
#include <string>
#include <chrono>
#include <fstream>
#include <thread>
// ----------------------
#include <cereal/archives/binary.hpp>
#include <cereal/access.hpp>
// ----------------------
#include <Rcpp.h>


class Primebase
{
	private:
	  int x;
	  double d;
	  
	  void simulate_heavy_work() {
	    std::this_thread::sleep_for(std::chrono::seconds(5));	    
	  }
	  
	  friend class cereal::access;
	  
	  template <class Archive>
	  void serialize(Archive& archive) {
	    archive(x, d);
	    return;
	  }
	  
	public:
	  // no args ctor
	  Primebase() : Primebase(42, 42.42) {
	    std::cout << "No-args ctor" << std::endl;
	    simulate_heavy_work();
	  };
	  
    // one arg ctor
    Primebase(int x) : Primebase(x, 42.42) {
      std::cout << "One arg ctor." << std::endl;
      simulate_heavy_work();
    }
	  
	  // three args ctor
	  Primebase(int x, double d) : x{x}, d{d} {
  		// simulate a long construction
  		simulate_heavy_work();
	  };
    
	  // dtor
	  ~Primebase() = default;
	  
	  // class member methods
	  int get_x() const { return x; }
	  double get_d() const { return d; }

};


// [[Rcpp::export]]
Rcpp::XPtr<Primebase> create_XPtr_to_Primebase(const int& x, const double& d) {
  Primebase* instance = new Primebase(x, d);
  return Rcpp::XPtr<Primebase>(instance);
}


// [[Rcpp::export]]
int dereference_xptr_int(Rcpp::XPtr<Primebase>& xptr) {
  return xptr.get()->get_x();
}

// [[Rcpp::export]]
double dereference_xptr_double(Rcpp::XPtr<Primebase>& xptr) {
  return xptr.get()->get_d();
}


// [[Rcpp::export]]
void serialize_Obj(Rcpp::XPtr<Primebase> xptr, std::string filename) {
  std::ofstream os(filename, std::ios::binary);
  cereal::BinaryOutputArchive archive(os);
  archive(*xptr.get());
  return;
}


// [[Rcpp::export]]
Rcpp::XPtr<Primebase> deserialize_Obj(std::string filename) {
  
  std::ifstream is(filename, std::ios::binary);
  cereal::BinaryInputArchive archive(is);
  
  // obj pointer is created based on no-args ctor
  Primebase* instance = new Primebase;

  archive(*instance); // creation from archive
  
  return Rcpp::XPtr<Primebase>(instance);
}
