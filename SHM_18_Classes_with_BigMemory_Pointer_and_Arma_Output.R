# In the following we create a C++ class which uses a big memory external
# pointer. The pointer is aiming towards hdd memory, where the data resides.
# The class self owns the dereferenced data as stack obj and becomes independent
# from the external pointer after construction.
# In this case we create an arma obj from the bigmemory pointer.
# ----------------------

# generate some toy data
library(bigmemory)
backing_path <- paste0(getwd(), "/Backend/")
file <- "bigTestMat"
if (!dir.exists(backing_path)) { dir.create(backing_path) }
if (!file.exists(paste0(backing_path, file, ".desc"))) {
  big_mat <- as.big.matrix(x = matrix(rnorm(n = 12), nrow = 3, ncol = 4),
                           type = "double", 
                           backingfile = paste0(file, ".bin"),
                           descriptorfile = paste0(file, ".desc"),
                           backingpath = backing_path
  )  
} else {
  big_mat <- attach.big.matrix(paste0(backing_path, file, ".desc"))
}
is.filebacked(big_mat)
big_mat[,]              # is a pointer

# big memory objs are no xptrs
xptr::is_xptr(big_mat)
# their address attribute is the external pointer
xptr::is_xptr(big_mat@address)

# either we create an external pointer here on the R side to the obj and pass it
# to the constructor, or we do it directly during class construction.
# We choose here the later approach.


library(Rcpp)
sourceCpp("SHM_18_Classes_with_BigMemory_Pointer_and_Arma_Output.cpp")
# NOTE: our class takes the big matrix as a SEXP, creates a XPtr, and creates
#       an arma matrix as class member form it.
#       The problem is that ginormous big matrices should  never be a stack obj.
#       In the next demo we consider this case.
# ------------------
external_mat_admin <- new(External_bigMatrix_Administration, big_mat@address)
external_mat_admin$print_Matrix()
external_mat_admin$return_Matrix()
external_mat_admin$add_Matrix_to_itself()
external_mat_admin$print_Matrix()

class(external_mat_admin$return_Matrix()) # matrix, no big matrix
rm(external_mat_admin)
gc()

xptr::is_null_xptr(big_mat@address)
