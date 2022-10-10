// Serialization Functions
// -------------------------------
// Since C++ lacks reflection, implementing serialization you to specify which
// data members should be serialized.
// 
// cereal supports single serialization functions (serialize) or split load/save
// pairs (load and save) either inside or outside of classes. Load and save
// functions can optionally be designated be designated as minimal
// (load_minimal and save_minmal).
// 
// Interal serialization functions can be kept private so long as cereal is
// given access by befriending creal::access. You can optionally choose to store
// versioning information by adding an additional parameter to your serialization
// functions (std::uint32_t const version) and using CEREAL_CLASS_VERSION macro.
// cereal will tell you if you've made a mistake at compile time, if possible.
// 
// Types of Serialization functions:
// Either internal or external. Functionality can either be in a single serialize
// function, or a split load and save function. Load and save functions can
// optionally be made to emit minimal representations.
// 
// When possible, it is preferred to use a single internal serialize method, 
// though split methods can be used when it is necessary. Unlike Boost, there is
// no need to explicitly tell cereal that it needs to use the split load-save
// pair.
// cereal will pick whichever is present and give a compile time error if it
// cannot disambiguate a single serialization method.
// 

// https://uscilab.github.io/cereal/serialization_functions.html
// Internal split/load functions:
// ------------------------------------
struct MyClass
{
  int x, y, z;
  
  template<class Archive>
  void save(Archive& archive) const // save fcts are const, o.w., throw static assertion error
  {
    archive(x, y, z);
  }
  
  template<class Archive>
  void load(Archive& archive)
  {
    archive(x, y, z);
  }
  
};




// External split/load functions:
// 
// 
// Non-public serialization
// 
// 
// Minimal split serialization
// 
// 
// Explict versioning
// 
// 
// Inheritance
// 
#include <Rcpp.h>
