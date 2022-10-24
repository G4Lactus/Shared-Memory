# Create a C++ class which stores just a member pointer of a data set, rather
# a member copy of the data. Inside the Rcpp function Test_Matrix_Administration
# several operations with this class are performed.
# 
# Unfortunately this kind of class definition does not deliver any usefulness
# in R. Our class operates on a raw pointer known to C++ but not to R.
# Back in R we obtain either garbage data or thrash, or even a crash of R
# Studio. As before in the functions demo, we need to use XPtr or bigM ptr.
# Therefore, Test_Matrix_Administration returns an XPtr to R.
# -------------------------------
mat <- matrix(rnorm(12), 3, 4)
print(mat)
library(Rcpp)
sourceCpp("SHM_12_Classes_with_Raw_Pointer.cpp")

xptr_MatAdmin <- Test_Matrix_Administration(mat, TRUE)
xptr::is_xptr(xptr_MatAdmin)
# all operations were conducted based on pointed to data, therefore the original
# data changed.
print(mat)
reload_pointer_data_for_Matrix_Administration(xptr_MatAdmin)
print(mat)

# release memory
rm(xptr_MatAdmin)
gc()


# As mentioned above just based on raw pointers the return does not work. But it
# is ideally suited to brutally crash your session, as memory access rights and
# reading are brutally harmed.
mat_admin <- new(Matrix_Administration, mat, TRUE)
mat_admin$print_Matrix()
# in the following we see: we can't return a raw pointer to R, but the C++
# operation is carried out
mat_admin$add_42_to_Matrix()
mat_admin$print_Matrix()
mat_admin$retrieve_ptr_address()
