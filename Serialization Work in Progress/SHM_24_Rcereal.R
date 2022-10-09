# Rcereal - cereal, A c++11 library for serialization, for R
# ----------------------------------------------------------------------
# https://cran.rstudio.com/web/packages/Rcereal/vignettes/intro.html
# 
# Provides R access to cereal header files. cereal is a header only C++11 
# serialization library that takes arbitrary data types and reversibly turns
# them into different representations, such as compact binary encodings, XML, 
# JSON.
# 
# For more information visit:
# http://uscilab.github.io/cereal/
# -------------------------------------------------

# Best practice: use the GitHub version
devtools::install("wush978/Rcereal")
Rcereal::update_version()
# in 2022: part of CRAN


# Getting Started:
# ---------------
library(Rcpp)
sourceCpp("SHM_24_Rcereal.cpp")
raw_vector <- serialize_myclass(1, 2, 4)
deserialize_myclass(raw_vector)
