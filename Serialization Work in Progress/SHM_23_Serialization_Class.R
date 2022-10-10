# Serialize and Deserialize a C++ Object in Rcpp
# --------------------------------------------------
# https://gallery.rcpp.org/articles/rcpp-serialization/
#
# In the following we investigate how to serialize a C++ object into a R raw
# object, and how to deserialize it again.
# --------------------------------------------
Rcpp::sourceCpp("SHM_23_Serialization_Class.cpp")

v <- serialize_MyClass(x = 1, y = 2, z = 4)
head(v)

deserialize_MyClass(v)
