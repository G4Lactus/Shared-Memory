# In the following we create a C++ class which uses a big memory external
# pointer. Furthermore our class owns only a raw pointer to the data along with
# a conversion construction towards an arma matrix.
# Inside the class we create another heap obj to deal with bigger data scenarios
# where it does not make sense to store the obj as stack obj.
# ----------------------

# generate some toy data
# -----------------------
library(bigmemory)

backing_path <- paste0(getwd(), "/Backend/")

very_big_Matrix <- FALSE # TRUE creates a 74.5 GB file
if (very_big_Matrix) {
  file <- "verybigTestMat"
  if (!dir.exists(backing_path)) { dir.create(backing_path) }
  if (!file.exists(paste0(backing_path, file, ".desc"))) {
    big_mat <- filebacked.big.matrix(
      nrow = 2e3,
      ncol = 5e6,
      init = 42,
      type = "double",
      backingfile = paste0(file, ".bin"),
      descriptorfile = paste0(file, ".desc"),
      backingpath = backing_path
    )
  } else {
    big_mat <- attach.big.matrix(paste0(backing_path, file, ".desc"))
  }  
} else {
  file <- "bigTestMat"
  if (!dir.exists(backing_path)) { dir.create(backing_path) }
  if (!file.exists(paste0(backing_path, file, ".desc"))) {
    big_mat <- as.big.matrix(
      x = matrix(1:1e3, nrow = 20, ncol = 50),
      nrow = 20,
      ncol = 50,
      type = "double",
      backingfile = paste0(file, ".bin"),
      descriptorfile = paste0(file, ".desc"),
      backingpath = backing_path
    )
  } else {
    big_mat <- attach.big.matrix(paste0(backing_path, file, ".desc"))
  }
}
is.filebacked(big_mat)


library(Rcpp)
sourceCpp("SHM_19_Classes_with_BigMemory_Pointer_and_ArmaPointer_Output.cpp")

external_mat_admin <- new(External_bigMatrix_Administration, big_mat@address)
external_mat_admin$print_Matrix()
external_mat_admin$return_Matrix()
external_mat_admin$add_Matrix_to_itself()
external_mat_admin$print_Matrix()
external_mat_admin$return_Matrix()
class(external_mat_admin$return_Matrix()) # matrix, no big matrix

# As we have set a pointer to the big matrix, all changes made to the matrix
# are made to the original data as well.
# However, the big advantage over the stacked version is that the external
# big matrix is on the hard drive rather the ram, and the class does not own
# the data any more, but only a pointer to them.

rm(external_mat_admin)
gc()
