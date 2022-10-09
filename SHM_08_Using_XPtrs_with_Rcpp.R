# Illustration of XPtrs and Armadillo matrices arma::mat
# -----------------------------------------------------------------------------
# Previously we demonstrated that raw pointers can't be returned to R.
# The fitting tool to do so are external pointers (XPtr).
# 
# In the following we create a matrix in R, move it to C++, and move a pointer
# from C++ back to R and work with this pointer from R, pass it back to C++ and
# return values.
# XPtrs are used to save data in the heap for later use in the same session.
#
# The full story is here:
# https://lists.r-forge.r-project.org/pipermail/rcpp-devel/2015-June/008806.html
# 
# Also see:
# http://arma.sourceforge.net/docs.html
# for constructor information of Armadillo matrices from pointers
# -----------------------------------------------------------------------------
library(Rcpp)
sourceCpp(file = "SHM_08_Using_XPtrs_with_Rcpp.cpp")

set.seed(42)
A <- matrix(rnorm(10), nrow = 5)
print(A)

# return ptrs to an Armadillo matrix
xptr_A1 <- export_to_Xptr_SEXP(A)
xptr_A2 <- export_Xptr(A)
# return ptr to a std vector in C++
xptr_some_vec <- export_Cpp_vec_to_R()
print(xptr_some_vec)

# check if the objs are xptrs :)
# you may need library(xptr)
xptr::is_xptr(xptr_A1)
xptr::is_xptr(xptr_A2)
xptr::is_xptr(xptr_some_vec)

# in your current session you can use these heap objs outside of R's memory
# and garbage collection.
# 
# NOTE: YOU CAN'T CALL THE EXTERNAL POINTERS FROM ANOTHER SESSSION.
# To do so you have to SERIALIZE them!

# data retrieval
vec <- retrieve_vec_from_xptr(xptr_some_vec)
print(vec)

# based on input SEXP from R to C++ the matrix is recovered
retrieve_mat_from_SEXP(xptr_A1, 5, 2)
retrieve_mat_from_SEXP_no_paras(xptr_A1)
retrieve_mat_from_SEXPptr(xptr_A1)

# just passing an R obj to a function that requires an XPtr is not enough
# xptr_A2 is at this point an SEXP obj
retrieve_mat_from_xptr(xptr_A2)

# but retrieving from direct XPtr input is possible
retrieve_mat_from_xptr(export_Xptr(A))
retrieve_mat_from_xptr1(export_Xptr(A))


