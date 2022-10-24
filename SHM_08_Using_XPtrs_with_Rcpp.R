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
# The documentation of XPtr constructs:
# https://dirk.eddelbuettel.com/code/rcpp/html/classRcpp_1_1XPtr.html
# 
# Also see:
# http://arma.sourceforge.net/docs.html
# for constructor information of Armadillo matrices from pointers
# -----------------------------------------------------------------------------

library(Rcpp)
sourceCpp(file = "SHM_08_Using_XPtrs_with_Rcpp.cpp")

# create some toy data
set.seed(42)
A <- matrix(rnorm(50), nrow = 5)
print(A)

# return an external ptr an armadillo matrix as SEXP
xptr_A1 <- export_mat_to_Xptr_SEXP(A, TRUE)  # pointer registers own memory
xptr_A2 <- export_mat_to_Xptr_SEXP(A, FALSE) # pointer does not register own memory

retrieve_mat_from_SEXPptr(xptr_A1)
retrieve_mat_from_SEXPptr(xptr_A2)


# return an external ptr to an armadillo matrix
xptr_A3 <- export_mat_to_XPtr_copy(A, TRUE)  # pointer registers own memory
xptr_A4 <- export_mat_to_XPtr_copy(A, FALSE) # pointer does not register own memory

retrieve_mat_from_XPtr(xptr_A3)
retrieve_mat_from_XPtr(xptr_A4) # garbage

# but retrieving from direct XPtr without creating an SEXP obj is possible
retrieve_mat_from_XPtr(export_mat_to_XPtr_copy(A, FALSE))

# now with reference to original object
xptr_A5 <- export_mat_to_XPtr_reference(A, TRUE)
xptr_A6 <- export_mat_to_XPtr_reference(A, FALSE)
retrieve_mat_from_XPtr(xptr_A5)
retrieve_mat_from_XPtr(xptr_A6)

# now we create a std vector in C++ and return it to R as xptr
xptr_some_vec <- export_Cpp_vec_to_R()
retrieve_vec_from_xptr(xptr_some_vec)

# inside your current session you can use these heap objs outside of R's garbage
# collection.
# NOTE: YOU CAN'T CALL THE EXTERNAL POINTERS FROM ANOTHER R SESSION.
# To do so you have to SERIALIZE them!

# Finally we demonstrate "later usage" of data stored in external pointers

# First, creating copies of R objs does not change the pointer address
xptr_A11 <- xptr_A1
xptr_A12 <- xptr_A1
library(xptr)
xptr::xptr_address(xptr_A1)
xptr::xptr_address(xptr_A11)
xptr::xptr_address(xptr_A12)
# Therefore creating new copies is redundant

# Second, passing the xptr back to the C++ backend and working with the data
work_with_xptr_stored_data(xptr_A1, TRUE)
work_with_xptr_stored_data(xptr_A1, FALSE)

work_with_xptr_stored_data(xptr_A2, TRUE)
work_with_xptr_stored_data(xptr_A2, FALSE)

work_with_xptr_stored_data(xptr_A3, TRUE)
work_with_xptr_stored_data(xptr_A3, FALSE)

work_with_xptr_stored_data(xptr_A4, TRUE)  # total garbage
work_with_xptr_stored_data(xptr_A4, FALSE) # total garbage

work_with_xptr_stored_data(xptr_A5, TRUE)  # works
work_with_xptr_stored_data(xptr_A5, FALSE) # works, except for final pointer B

# operating on a reference works in both cases, but A is corrupted
work_with_xptr_stored_data(xptr_A6, TRUE)  # works
work_with_xptr_stored_data(xptr_A6, FALSE) # works, except for final pointer B

