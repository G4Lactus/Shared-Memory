# In this part we use shared pointers from the smart pointer family.
# 
Rcpp::sourceCpp("SHM_16_Classes_with_smart_shared_and_XPointer.cpp")

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
external_mat_admin$return_use_count() 

# object no longer needed: delete it, release pointers and memory
rm(external_mat_admin)

# check out the obj
xptr::xptr_address(xptr_mat)
print(xptr_mat)

# release memory
xptr::xptr_clear(xptr_mat)
xptr::xptr_address(xptr_mat)
print(xptr_mat)
xptr::is_null_xptr(xptr_mat)
# the memory is now released, delete pointer obj
rm(xptr_mat)
gc()

# NOTE: if we delete XPtr obj before external_mat_admin obj and perform a gc()
#       the class is still operational and we don't produce a crash, as the
#       class' shared pointer is still active.
