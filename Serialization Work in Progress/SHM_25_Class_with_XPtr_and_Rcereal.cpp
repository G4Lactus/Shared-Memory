#include <iostream>
#include <chrono>
#include <memory>
#include <fstream>
#include <thread>
// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(Rcereal)]]
#include <Rcpp.h>
#include <cereal/archives/binary.hpp>



class Primebase
{
private:
  std::unique_ptr<int> x;

public:
  
  // ctor
  Primebase() : x{nullptr} {}
  
  Primebase(int x_) : x{std::unique_ptr<int>(new int {x_})} {
    // simulate a long construction
    std::this_thread::sleep_for(std::chrono::seconds(5));
  };
  
  // dtor
  ~Primebase() = default;

  // class member methods
  int answer() { return *x; }
  
  template <class Archive>
  void serialize(Archive& archive) {
    archive(x);
    return;
  }
};


// [[Rcpp::export]]
Rcpp::XPtr<Primebase> create_XPtr_to_Primebase(const int& x) {
  Primebase* instance = new Primebase(x);
  return Rcpp::XPtr<Primebase>(instance);
}


// [[Rcpp::export]]
int dereference_xptr(Rcpp::XPtr<Primebase> xptr) {
  return xptr.get()->answer();
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
  
  Primebase* instance = new Primebase; 
  archive(*instance); // creation from archive
  
  return Rcpp::XPtr<Primebase>(instance);
}

