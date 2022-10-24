# In this scenario, compared to the 13th, we consider the case where the class
# does not own the data itself, but just an pointer to them.
# ----------------------------
Rcpp::sourceCpp("SHM_14_Classes_with_XPointer.cpp")

# create external pointer obj to our matrix and project data to the heap
xptr_mat <- create_XPtr_for_R_obj(matrix(rnorm(12), 3, 4))

# check out the obj
xptr::xptr_address(xptr_mat)
print(xptr_mat)


# use external pointer as input to class obj creation where the
# the class owns a copy of the external pointer to the obj
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

# now we return the classes' owned pointer to the data as xptr to R
xptr_to_class_instance <- external_mat_admin$return_pointer_to_Data() 
xptr::xptr_clear(xptr_to_class_instance)
xptr::null_xptr(xptr_to_class_instance)
# object no longer needed: delete it, release pointers and memory
rm(external_mat_admin)
gc()

# xptr_mat survived the gc, check out the obj
xptr::xptr_address(xptr_mat)

# release memory
xptr::xptr_clear(xptr_mat)
xptr::null_xptr(xptr_mat)
xptr::xptr_address(xptr_mat)

# the memory is now released, delete pointer obj
rm(xptr_mat)
gc()

# NOTE: the External_Matrix_Administration objs is still running(!) if we delete
#       the external pointer obj and call the class obj again. The class owns a
#       copy of the external pointer.
# ------------------------------------