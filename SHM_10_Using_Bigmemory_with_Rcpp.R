# Using bigmemory with Rcpp
# -----------------------------------------------------------------------------
# When accessing a big.matrix object in Rcpp, there are two objects you are
# interested in creating. First, the External Pointer for the big.matrix, i.e.,
# XPtr<BigMatrix>, which also stores all of the attributes of the big.matrix,
# including nrow(), ncol(), and matrix_type().
# 
#
# The second is the MatrixAccessor which allows you to access elements within
# the big.matrix. When creating the MatrixAccessor you must declare the type
# of the BigMatrix.
# 
# 
# A BigMatrix object stores elements in a column major format, meaning that
# values are accessed and filled in by column, rather than by row.
# The MatrixAccessor implements the bracket operator, returning a pointer to
# the first element of a column. 
# As a result, for a MatrixAccessor mat, the i-th row and j-th column is 
# accessed with m[j][i] rather than m[i, j],
# ---------------------------------------------------
library(bigmemory)
library(microbenchmark)
library(Rcpp)
sourceCpp("SHM_10_Using_Bigmemory_with_Rcpp.cpp")

# Generate some data
# --------------------------------------------------------------
nrows <- 1e4
path_to_backend <- paste0(getwd(), "/Backend/")
if (!dir.exists(path_to_backend)) { dir.create(path_to_backend) }
file <- "big_Mat"
full_path <- paste0(path_to_backend, file, ".desc")
if (!file.exists(full_path)) {
  big_Mat <- filebacked.big.matrix(nrow = 1e4, ncol = 3, type = "double",
                                   backingfile = paste0(file, ".bk"), 
                                   backingpath = path_to_backend,
                                   descriptorfile = paste0(file, ".desc"),
                                   dimnames = c(NULL, NULL))  
} else {
  big_Mat <- attach.big.matrix(full_path)
}

# populate matrix with data
set.seed(42)
for (i in 1:(bigmemory::ncol(big_Mat))) {
  big_Mat[, i] <- rnorm(nrows)*i
}
big_Mat[,]



# Example 1: templated big column sum for column X
# -----------------------------------------------------------------------------
big_colSums(big_Mat@address, 42)
sum(big_Mat[,3])
all.equal(sum(big_Mat[,3]), big_colSums(big_Mat@address, 3))
all.equal(colSums(big_Mat[,]), sapply(1:3, function(x) { return(big_colSums(big_Mat@address, x)) }))

set.seed(42)
mat  <- matrix(rnorm(60*1e6), nrow = 60)
ridx <- sample(ncol(mat), size = 100, replace = FALSE)
bigm <- as.big.matrix(mat)
options(width = 100)
microbenchmark(
  res1 <- colSums(mat[, ridx]),
  res2 <- sapply(ridx, function(x) { return(big_colSums(bigm@address, x)) }),
  times = 1e4
)
# Of course the example is not meaningful, but it illustrates how to work with
# templates in Rcpp.



# Example 2: templated big row sums
# -----------------------------------------------------------------------------
# https://stackoverflow.com/questions/24687392/computing-row-sums-of-a-big-matrix-in-r
# and inspired from
# https://gallery.rcpp.org/articles/using-bigmemory-with-rcpp/
# ----------------------------------------
big_rowSums(big_Mat@address)
rowSums(big_Mat[,])
print(all.equal(big_rowSums(big_Mat@address), rowSums(big_Mat[,])))



# Example 3: calculate total sum of a matrix
# -----------------------------------------------------------------------------
sum_bigMat(big_Mat@address, nrow(big_Mat), ncol(big_Mat))
sum(big_Mat[,])
print(all.equal(sum_bigMat(big_Mat@address, nrow(big_Mat), ncol(big_Mat)), sum(big_Mat[,])))



# Example 4: casting a big matrix to an armadillo matrix
# -----------------------------------------------------------------------------
armacast(big_Mat@address, TRUE)
armacast(big_Mat@address, FALSE)



# Example 5:
# -----------------------------------------------------------------------------
colSum_bm(big_Mat@address)
colSums(big_Mat[])
colSum_bm(bigm@address)
colSums(big_Mat[])



# Example 6: change all non.zero numbers to a constant
# -----------------------------------------------------------------------------
# Inspiration:
# https://stackoverflow.com/questions/49960165/change-all-nonzero-numbers-in-big-matrix-to-1
# ----------------------------------------------------------
big_nz_to_const(big_Mat@address, 42)
big_Mat[]
flush(big_Mat)



# Example 7:
# -----------------------------------------------------------------------------
# https://stackoverflow.com/questions/38569321/extracting-a-column-with-nas-from-a-bigmemory-object-in-rcpp
# ---------------
bm <- bigmemory::as.big.matrix(x = as.matrix(reshape2::melt(matrix(c(1:4, NA, 6:20), 4, 5))))
bm[,]
bigmemory::typeof(bm) # you get integer!

# IEEE 754standard has NaN defined for floating point only
# The fix is simple enough: use IntegerVector
get_filterColumn(bm@address, 0)
bm[get_filterColumn(bm@address, 0),]

get_filterColumn(bm@address, 1)
bm[get_filterColumn(bm@address, 1),]

get_filterColumn(bm@address, 2)
bm[get_filterColumn(bm@address, 2),]

bm[,inds_to_keep(bm@address)]

bigMat_impute_NA(bm@address)



# Example 8: transpose a big matrix
# -----------------------------------------------------------------------------
set.seed(42)
bigm <- as.big.matrix(matrix(rnorm(12), nrow = 4))
bigm[,]

# create a new pointer to a big matrix file according to the dimensions
t_big <- big.matrix(nrow = ncol(bigm), ncol = nrow(bigm), type = "double")
t_big[,]

transpose_bigMat(bigm@address, t_big@address)
t_big[,]



# Example 9: Perform matrix*vector product
# -----------------------------------------------------------------------------
set.seed(42)
bigm <- as.big.matrix(matrix(rnorm(12), nrow = 4), type = "double")
prod_BigMat_with_vector(bigm@address, c(1, 2,3))



# Example 10: Operate on the big matrix obj
# -----------------------------------------------------------------------------
set.seed(42)
bigm <- as.big.matrix(matrix(seq(21), 7,3), type = "integer")
bigm[,]
local_changes_on_bigMat(bigm@address)
bigm[,]



# Example 11: 
# -----------------------------------------------------------------------------
set.seed(42)
bigm <- as.big.matrix(matrix(seq(25), 5, 5), type = "integer")
bigm[,]
get_bigMat_diag(bigm@address)


