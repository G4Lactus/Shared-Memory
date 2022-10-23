# Starting from this part we want to make active use of shared memory by means
# of pointers and C++. We start with C++ raw pointers.
# ------------------------
library(Rcpp)
sourceCpp("SHM_07_Using_RawPtrs_with_Rcpp.cpp")

# generate some toy data
set.seed(42)
A <- matrix(rnorm(12), nrow = 4, ncol = 3)
print(A)

# we define the function return_mat() in Rcpp, take it as a reference from R
# and beam it as a pointed to variable through some pipeline.
simulate_rawPtr_Usage(A, 2)
print(A)
# though we worked with a reference no changes are in A because we defined a
# new pointer to the data.
simulate_rawPtr_Usage(A, 1)
print(A)
