// Investigate how to add serialization methods for your classes
struct MyClass
{
  int x, y, z;
  
  // This method lets cereal know which data members to serialize
  template<class Archive>
  void serialize(Archive& archieve)
  {
    archive(x, y, z); // serialize things by passing them to the archive
  }
  
};


// Serialize your data
// Archives generally take either an std::istream or std::ostream
#include <cereal/binary.hpp>

int main() {
  
  std::stringstream ss; // any stream can be used
  
  {
    cereal::BinaryOutputArchive oarchive(ss); // create an output archive
    MyData m1, m2, m3;
    oarchive(m1, m2, m3); // write the data to the archive
  }
  
  {
    cereal::BinaryInputArchive iarchive(ss);
    MyData m1, m2, m3;
    iarchive(m1, m2, m3); // read the data from the archive
  }
}


// Naming values
#include <cereal/archives/xml.hpp>
#include <fstream>

int main() {
  
  {
    std::ofstream os("data.xml");
    cereal::XMLOutputArchive archive(os);
    
    MyData m1;
    int someInt;
    double d;
    
    archive(CEREAL_NVP(m1), // names the output the same as the variable name
            someInt,        // no nvp - cereal will automatically generate an enumerated name
            cereal::make_nvp("this_name_is_way_better", d); // specify a name of your choosing
           )
  }
  
  
  {
    std::ifstream is("data.xml");
    cereal::XMLInputArchive archive(is);
    
    MyData m1;
    int someInt;
    double d;
    
    archive(m1, someInt, d); // NVPs not strictly necessary when loading but
                             // could be used (even out of order)
  }
  
  
  return 0;
}



