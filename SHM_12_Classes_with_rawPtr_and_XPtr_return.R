# Now we create a C++ class which stores just a member pointer of a data set,
# rather a member copy of the data.
# Inside the Rcpp function Test_Matrix_Administration several operations with
# this class are performed.
# 
# As we discovered in the functions part, we can't return a raw pointer to R, 
# but need XPtrs. Therefore Test_Matrix_Administration returns such a one.
#
# Additionally, our class is equipped with a member method to return the
# class instance as XPtr.
# 
# Additionally, we demonstrate how to release xptr and delete them safely.
# -------------------------------
mat <- matrix(rnorm(12), 3, 4)
print(mat)
library(Rcpp)
sourceCpp("SHM_12_Classes_with_rawPtr_and_XPtr_return.cpp")


xptr_MatAdmin <- test_Matrix_Administration(mat, TRUE)
xptr::is_xptr(xptr_MatAdmin)
# all operations were conducted based on pointed to data, therefore the original
# data changed.
print(mat)
reload_pointer_data_for_Matrix_Administration(xptr_MatAdmin)
print(mat) # no changes to mat, as we worked with copies

# release memory
xptr::xptr_clear(xptr_MatAdmin)
xptr::null_xptr(xptr_MatAdmin)
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

xptr_armaMat <- mat_admin$raw_to_xptr()
xptr::xptr_clear(xptr_armaMat)
xptr::null_xptr(xptr_armaMat)

xptr_mat_admin_instance <- mat_admin$return_class_to_xptr()
xptr::xptr_clear(xptr_mat_admin_instance)
xptr::null_xptr(xptr_mat_admin_instance)

# release memory
rm(mat_admin)
gc()
