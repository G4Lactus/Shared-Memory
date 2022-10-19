# This time the class uses a reference to the XPtr and does not own it itself.
# 
# More information about XPtr:
# https://dirk.eddelbuettel.com/code/rcpp/html/classRcpp_1_1XPtr.html
# ----------------------------------

Rcpp::sourceCpp("SHM_15_Classes_with_XPointer_Reference.cpp")

# create external pointer obj to our matrix
xptr_mat <- create_XPtr_for_R_obj(matrix(rnorm(12), 3, 4))
# check out the obj
print(xptr_mat)
xptr::xptr_address(xptr_mat)


# use external pointer as input to class obj creation where the
# the class uses the exact same data pointer to the obj
external_mat_admin <- new(External_Matrix_Administration, xptr_mat)
external_mat_admin$print_Matrix()
external_mat_admin$add_42_to_Matrix()
external_mat_admin$print_Matrix()
external_mat_admin$return_Matrix()
external_mat_admin$add_number_to_Matrix(42.42)
external_mat_admin$print_Matrix()
external_mat_admin$return_Matrix()
external_mat_admin$add_number_to_Matrix(42L)
external_mat_admin$print_Matrix()
external_mat_admin$return_Matrix()
# this time the return pointer is exactly the same as the input pointer
external_mat_admin$return_pointer_to_Data() 

# object no longer needed: delete it, release pointers and memory
rm(external_mat_admin)

# check out the obj
xptr::xptr_address(xptr_mat)
print(xptr_mat)

# release memory
xptr::xptr_clear(xptr_mat)
xptr::xptr_address(xptr_mat)
print(xptr_mat)
# the memory is now released, delete pointer obj
rm(xptr_mat)

# NOTE: the External_Matrix_Administration objs is still running(!) if we delete
#       the external pointer obj and call the class obj again. 
#       But if we perform a garbage collection (gc()) the memory address is lost.
#       Invoking the call from the class towards the data results into a 
#       crash.
