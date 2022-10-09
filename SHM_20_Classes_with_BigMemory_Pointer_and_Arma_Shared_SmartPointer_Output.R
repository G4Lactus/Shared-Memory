# In the following we create a C++ class which uses a big memory external
# pointer. Furthermore our class owns only a smart shared pointer to the data
# along with a conversion construction towards an arma matrix.
# ----------------------

# generate some toy data
library(bigmemory)
backing_path <- paste0(getwd(), "/Backend/")
file <- "bigTestMat"
if (!dir.exists(backing_path)) { dir.create(backing_path) }
if (!file.exists(paste0(backing_path, file, ".desc"))) {
  big_mat <- as.big.matrix(x = matrix(rnorm(n = 12), nrow = 3, ncol = 4),
                           type = "double", 
                           backingfile = paste0(file, ".bk"),
                           descriptorfile = paste0(file, ".desc"),
                           backingpath = backing_path
  )  
} else {
  big_mat <- attach.big.matrix(paste0(backing_path, file, ".desc"))
}
is.filebacked(big_mat)
big_mat[,]              # is a pointer
class(big_mat)

# big memory ptr are no xptrs
xptr::is_xptr(big_mat)

# even the address is no external pointer
xptr::is_null_xptr(big_mat@address)
xptr::is_null_xptr(big_mat)
# either we create an external pointer here on the R side to the obj, or we
# do it directly during class construction. We choose here the later approach.


# due to the missing xptr property we have to hand over the big matrix pointer
# as SEXP to C++
# ---------------
library(Rcpp)
sourceCpp("SHM_20_Classes_with_BigMemory_Pointer_and_Arma_Shared_SmartPointer_Output.cpp")


# NOTE: our class takes the big matrix as a SEXP, creates a XPtr, and creates
#       an arma matrix as class member form it.
#       The problem is that ginormous big matrices should  never be a stack obj.
#       In the next demo we consider this case.
# ----------------------------------
external_mat_admin <- new(External_bigMatrix_Administration, big_mat@address)
external_mat_admin$print_Matrix()
external_mat_admin$return_Matrix()
external_mat_admin$add_Matrix_to_itself()
external_mat_admin$print_Matrix()
external_mat_admin$return_Matrix()
class(external_mat_admin$return_Matrix()) # matrix, no big matrix

# compare how the external matrix evolved
big_mat[,]
# As we have set a pointer to the big matrix, all changes made to the matrix
# are made to the original data as well.
# However, the big advantage over the stacked version is that the external
# big matrix is on the hard drive rather the ram, and the class does not own
# the data any more, but only a pointer to them.

rm(external_mat_admin)

