#include <iostream>
#include <fstream>
#include <filesystem>
#include <memory>
#include <string>
// [[Rcpp::plugins("cpp17")]]
// [[Rcpp::depends(Rcereal)]]
#include <cereal/types/unordered_map.hpp>
#include <cereal/types/memory.hpp>
#include <cereal/archives/binary.hpp>
#include <Rcpp.h>


struct MyRecord
{
  uint8_t x, y;
  float z;
  
  template<class Archive>
  void serialize(Archive& ar) 
  {
    ar(x, y, z);
  }
};


struct SomeData
{
  int32_t id;
  std::shared_ptr<std::unordered_map<uint32_t, MyRecord>> data;
  
  template <typename Archive>
  void save(Archive& ar) const
  {
    ar(data);
  }
  
  template <typename Archive>
  void load(Archive& ar)
  {
    static int32_t idGen = 0;
    id = idGen++;
    ar(data);
  }
};


// [[Rcpp::export]]
int main() 
{

  std::filesystem::path cwd = std::filesystem::current_path();
  Rcpp::Rcout << cwd << std::endl;
  std::string file_name{"test_output.cereal"};
  std::string path_to_file{cwd.string() + "\\Backend\\" + file_name};
  std::ofstream os(path_to_file, std::ios::binary);
  cereal::BinaryOutputArchive archive(os);
  
  SomeData myData;
  archive(myData);
  
  return 0;
  
}