# Integration of bigstats and Rcpp
# --------------------------------------
# In the following we investigate the C++ bindings of bigmemory and bigstatsR.
# They easily allow you to use memory mapping technology in your Rcpp code.
# 
# bigstatsR:
# ----------
# https://cran.r-project.org/web/packages/bigstatsr/bigstatsr.pdf
# https://www.r-bloggers.com/2017/07/package-bigstatsr-statistics-with-matrices-on-disk-user-2017/
# https://privefl.github.io/bigstatsr/articles/bigstatsr-and-bigmemory.html
# ----------------------------

# Generate some data and bigmemory and FBM big matrix objs
set.seed(42)
data <- matrix(rnorm(25), 5, 5)
temp_dir <- tempdir()
file <- "bigX"
bigX <- bigmemory::as.big.matrix(data, type = "double", backingfile = paste0(file, ".bin"), 
                                 descriptorfile = paste0(file, ".desc"), backingpath = temp_dir)
fbmX <- bigstatsr::as_FBM(data, type = "double", backingfile = paste0(temp_dir, "\\", file, "_fbm.bk"))


# Rcpp Connection
# ----------------------------
# WARNING: bigmemory and bistatsr C++ interfaces are so called matrix accessors,
# their source code defines them based on std::auto_ptr, a deprecated concept of
# C++'s smart pointers. During compilation you will receive warnings.
# 
# C++ style:
# ----------
# bigmemory:  macc[j][i] , like in raw array notations
# FBM:        acc(i, j)  , like in modern C++
# ------------------------
library(Rcpp)
sourceCpp("SHM_09_bigmemory_and_bigstats_Rcpp.cpp")

# [bigmemory] pointer based
colSum_bm(bigX@address)
colSums(bigX[,])

# [bigstatsr] obj based
colSum_fbm(fbmX)
colSums(fbmX[])
colSum_subfbm(fbmX, bigstatsr::rows_along(fbmX), 1:5)
colSum_subfbm(fbmX, bigstatsr::rows_along(fbmX), 1:3)

# And of course the results coincide with base R.
