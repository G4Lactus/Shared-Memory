# In this scenario we pass the bigmatrix pointer directly to our class
# and we don't use Armadillo matrices, but instead we directly work on
# the data on disk. We can do so by means of a matrix or a submatrix
# accessor, as we have seen previously.
# However, this demands from us that we provide our own big matrix
# linear algebra. But all RAM problems are gone.
# ---------------------------------------------------------------

library(bigmemory)

set.seed(42)
backing_path <- paste0(getwd(), "/Backend/")
candidates <- c( "bigTestMat1", "bigTestMat2")

big_mat_lst <- lapply(candidates, function(candidateX) {
  path_to_file <- paste0(backing_path, candidateX, ".bk")
  
  if (!file.exists(path_to_file)) {
    
    big_mat <- as.big.matrix(x = matrix(
                                  data = rnorm(n = 12), 
                                  nrow = ifelse(candidateX == "bigTestMat1", 3, 4),
                                  ncol = ifelse(candidateX == "bigTestMat1", 4, 3)
                                  ),
                             type = "double",
                             backingfile = paste0(candidateX, ".bk"),
                             descriptorfile = paste0(candidateX, ".desc"),
                             backingpath = backing_path
    )
  } else {
    big_mat <- attach.big.matrix(obj = paste0(backing_path, paste0(candidateX, ".desc")))
  }
  
  return(big_mat)
})
names(big_mat_lst) <- paste0("big_mat", 1:length(big_mat_lst))


# Inspection
big_mat_lst$big_mat1 # is a pointer
big_mat_lst$big_mat1[,]
class(big_mat_lst$big_mat1)

big_mat_lst$big_mat2 # is a pointer
big_mat_lst$big_mat2[,]
class(big_mat_lst$big_mat2)


# big memory ptr are no xptrs
xptr::is_xptr(big_mat_lst$big_mat1)
# even the address is no external pointer
xptr::is_null_xptr(big_mat_lst$big_mat1@address)
xptr::is_null_xptr(big_mat_lst$big_mat1) # error - no xptr
print(big_mat_lst$big_mat1@address)
# due to the missing xptr property we have to hand over the big matrix pointer
# as SEXP to C++


library(Rcpp)
sourceCpp("SHM_22_Classes_with_BigMemory_Pointer_and_BigMemory_Output.cpp")
# NOTE: our class takes the big matrix 1 as pointer and operates based on it.
#       Armadillo matrices are not used now.
#       Also note, you would never return ginormous big matrices back to the R
#       stack and R workspace.
# -----------------------------
external_mat_admin <- new(External_bigMatrix_Administration, big_mat_lst$big_mat1@address)

external_mat_admin$get_head_bigMatrix(10)
head(big_mat_lst$big_mat1[,])

external_mat_admin$colSums_bigMatrix()
colSums(big_mat_lst$big_mat1[,])
identical(round(colSums(big_mat_lst$big_mat1[,]), 8), round(external_mat_admin$colSums_bigMatrix(), 8))

external_mat_admin$rowSums_bigMatrix()
rowSums(big_mat_lst$big_mat1[,])
identical(round(rowSums(big_mat_lst$big_mat1[,]), 8), round(external_mat_admin$rowSums_bigMatrix(), 8))

external_mat_admin$colMeans_of_bigMatrix()
colMeans(big_mat_lst$big_mat1[,])
identical(round(colMeans(big_mat_lst$big_mat1[,]), 8), round(external_mat_admin$colMeans_of_bigMatrix(), 8))

external_mat_admin$get_head_bigMatrix(10)
external_mat_admin$add_number_to_bigMatrix(42)
external_mat_admin$get_head_bigMatrix(10)
external_mat_admin$colDifference_bigMatrix()

external_mat_admin$demean_bigMatrix()
external_mat_admin$get_head_bigMatrix(10)

external_mat_admin$colStandardDeviation_of_bigMatrix(TRUE)
external_mat_admin$colStandardize_bigMatrix(TRUE)
external_mat_admin$get_head_bigMatrix(10)

external_mat_admin$multiplay_scalar_with_bigMatrix(42.42)
external_mat_admin$get_head_bigMatrix(10)

external_mat_admin$filter_bigMatrix_cols_for_same_column_number()
external_mat_admin$get_bigM_ptr() # NOTE: exact same pointer as big_mat

external_mat_admin$get_head_bigMatrix(10)
external_mat_admin$add_bigMatrix_to_new_bigMatrix(big_mat_lst$big_mat1@address)
external_mat_admin$get_head_bigMatrix(10)
external_mat_admin$subtract_bigMatrix_to_new_bigMatrix(big_mat_lst$big_mat1@address)

big_mat_lst$big_mat_res <- big.matrix(nrow = nrow(big_mat_lst$big_mat1), ncol = ncol(big_mat_lst$big_mat2), init = 0)
external_mat_admin$get_head_bigMatrix(10)
big_mat_lst$big_mat_res[,] <- 0
big_mat_lst$big_mat1[,] %*% big_mat_lst$big_mat2[,]
external_mat_admin$multiply_bigMatrix_with_bigMatrix(big_mat_lst$big_mat2@address, big_mat_lst$big_mat_res@address)
big_mat_lst$big_mat_res[,]

rm(external_mat_admin)

