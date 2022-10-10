# From now on we focus on (de-) serialization of objects.
# We use cereal which is a C++11 library for serialization of objects
# https://uscilab.github.io/cereal/
# 
# It is a header only library and takes arbitrary data type and reversibly
# turns them inot different representations, such as compact binary encodings,
# XML, or JSON. It was designed as fast, lightweight, and easy to extend.
# It has no external dependencies.
# 
# Features are full support of polymorphism and inheritance. However, it is
# a lightweight and performs not the same level of object tracking as Boost.
# As a consequence raw pointers and references are not supported, but smart 
# pointers (std::shared_ptr and std::unique_ptr).
# 
# cereal uses features from C++11 and requires a compliant compiler. It also
# supports g++ 4.7.3, clang++ 3.3, ad MSVC2013 and newer.
# 
# cereal offers more flexible ways of writing serialization functions such as
# moving them outside of class definitions or splitting them into separate load
# and save functions.
# You can read all about that in the serialization functions section of the
# documentation. 
# cereal can also support class versioning, private serialization methods, and
# even classes that don’t support default construction.
# You can serialize primitive data types and nearly every type in the standard
# library without needing to write anything yourself.
#
# Currently (2022) cereal supports three basic archive types:
# - binary
# - XML
# - JSON
# 
# #include <cereal/archives/binary.hpp>
# #include <cereal/archives/portable_binary.hpp>
# #include <cereal/archives/xml.hpp>
# #include <cereal/archives/json.hpp>
# -------------------------------------------------------

library(Rcpp)