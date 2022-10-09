# Now we create a C++ class, but pass the data as XPtr from R to the class
# constructor. The problems we encountered with the rawpointer are gone 
# immediately.
# 
# For more info about XPtr see:
# https://dirk.eddelbuettel.com/code/rcpp/html/classRcpp_1_1XPtr.html
# ------------------------
Rcpp::sourceCpp("SHM_12_Classes_with_XPointer.cpp")

# create external pointer obj to our matrix
xptr_mat <- create_XPtr_for_R_obj(matrix(rnorm(12), 3, 4))

# check out the obj
xptr::xptr_address(xptr_mat)
print(xptr_mat)
xptr::is_xptr(xptr_mat)

# use external pointer as input to class obj creation where the 
# class owns the obj as the external pointer is de-referenced
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


# check out the obj
xptr::xptr_address(xptr_mat)
print(xptr_mat)

# release memory
xptr::xptr_clear(xptr_mat)
xptr::xptr_address(xptr_mat)
print(xptr_mat)
# the memory is now released, delete pointer obj
rm(xptr_mat)

# NOTE: the External_Matrix_Administration obj is still running(!) as the class
#       owns a copy of the data from the dereferenced pointer.
# -------------------------------
