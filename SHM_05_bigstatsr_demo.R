#' Tutorial: bigstatsr
# -----------------------------------------------------------------------------
#' In the following `bigstatsr` is investigated. It is a modern package for out-
#' of-memory statistical analysis. It provides a file-backed data structure,
#' the FBM (Filebacked Big Matrix), that is a powerful and often more intuitive
#' alternative to tehose in `bigmemory` and `ff`.
#'
#' Key advantages of `bigstatsr`:
#'
#' - **Modern API:** Works directly with the FBM object, instead of
#'  managing pointers.
#'
#' - **Simplified File Management:** Backing files are easiert o manage.
#'  Removing the R object and running `gc()` is often all that is needed.
#'
#' - **Rich Algorithmic Suite:** The package includes a wide range of highly
#'  optimized algorithms for common statistical tasks (SVD, linear models, etc.)
#'
#' - **Effortless Parallelism:** Most functions are multi-threaded by default.
#'
#' Also bigstats offers a C++ backend for own code.
#'
#' The author's github:
#' https://github.com/privefl/bigstatsr
#'
#' And some tutorial:
#' https://privefl.github.io/R-presentation/bigstatsr.html#19
#'
# -----------------------------------------------------------------------------

library(bigstatsr)
library(ggplot2)


#' Demo 1: The Filebacked Big Matrix (FBM)
# -----------------------------------------------------------------------------
#' The FBM is the core data structure of `bigstatsr`. It creates two files on
#' disk: a `.bk` file for the binary data and an `.rds` file for metadata.
#'

#' Setup: Create a temporary directory for the backing files
backing_dir <- file.path(tempdir(), "bigstatsr_demo")
dir.create(backing_dir)
backing_file_path <- file.path(backing_dir, "my_fbm")

#' Create FBM
big_X <- bigstatsr::FBM(
  nrow = 1e3, ncol = 1e4,
  init = 0, backingfile = backing_file_path
)


#' The R object itself is small, just a reference to the on-disk files.
print(big_X)
object.size(big_X)
file.size(big_X$backingfile) / (1024 ** 2) # ~80 MB


#' Big Apply Methods
#' `big_apply` processes the matrix in blocks, making it memory-efficient.
bigstatsr::big_apply(big_X, a.FUN = function(X, ind) {
  # `X`: the FBM object
  # `ind`: a vector of column indices for the current blocks
  X[, ind] <- stats::rnorm(nrow(X) * length(ind))
  return(NULL)
})

# Acces data using standard R syntax
big_X[1:5, 1:5]



#' Demo 2 : High-Performance Algorithms
# -----------------------------------------------------------------------------
#' Like `bigmemory`, the big strength of `bigstatsr` is its suite of
#' optimized algorithms that operate directly on FBM objects without
#' loading them into RAM.

#' 1. Correlation matrix
# -----------------------------------------
#' `big_cor` is significantly faster than base R's `cor()` for large matrices
#' and operates directly on a FBM object.

#' For comparison:.combine
#' First create R stack object than compute the correlation matrix
system.time(corr1 <- cor(big_X[]))

#' Now apply the big matrix function, its returns another FBM object
system.time(corr2 <- bigstatsr::big_cor(big_X))

#' Check
all.equal(corr1, corr2[])


#' 2. Singular Value Decomposition (SVD)
# -----------------------------------------
#' `big_SVD` is optimized for tall-and-skinny or short-and-fat matrices.

# For comparison: base::svd
system.time(svd1 <- svd(scale(big_X[]), nu = 10, nv = 10))

# vs. bigstatsr::big_SVD()
system.time(svd2 <- bigstatsr::big_SVD(
  big_X,
  fun.scaling = bigstatsr::big_scale(),
  k = 10
))


#' 3. Genome-Wide Association Study (GWAS) Simulation
# ----------------------------------------------------
#' Simulate a phenotype `y` that is influenced by a subset of columns
#' in `big_X`.
set.seed(42)
causal_indices <- sample(ncol(big_X), 100)
y <- big_X[, causal_indices] %*% rnorm(100)
y <- y + rnorm(length(y), sd = 2 * sd(y))

svd_big_X <- bigstatsr::big_SVD(big_X)

# `big_univLinReg` performs a massive number of linear regressions in parallel.
# We include the top SVD components as covariates to control for structure.
assoc_test <- bigstatsr::big_univLinReg(big_X, y, covar.train = svd_big_X$u)

# A Manhattan plot is a standard way to visualize these results.
plot(assoc_test, type = "Manhattan") +
  aes(color = cols_along(big_X) %in% causal_indices) +
  labs(color = "Is Causal?")



#' 4. Prediction with Regularized Regression
# ---------------------------------------------------
#' Split the indices in train/test sets
ind_train <- sort(sample(nrow(big_X), size = 0.8 * nrow(big_X)))
ind_test <- setdiff(rows_along(big_X), ind_train)

#' Train a sparse linear model (Elastic-Net) on the FBM.
sp_mod <- bigstatsr::big_spLinReg(
  big_X, y[ind_train], ind.train = ind_train,
  covar.train = svd_big_X$u[ind_train, ],
  alphas = c(1, 0.1, 0.01)
)

#' Predict on the test set and evaluate the results
pred <- predict(sp_mod, X = big_X,
  ind.row = ind_test, covar.row = svd_big_X$u[ind_test, ]
)

#' QQPlot: true vs prediction
qplot(pred, y[ind_test]) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Predicted Values", y = "True Values") +
  theme_bigstatsr()



#' Demo 3: Parallelism and Sharing
# ---------------------------------------------------
#' Most `bistatsr` functions are already parallelized based on `openMP`.
#' Given `openMP` is available on the system, simplly specify the number
#' of cores. Without `openMP` better do single threading.
#'
ncores <- bigstatsr::nb_cores()
system.time(bigstatsr::big_univLinReg(big_X, y, ncores = 1))
system.time(bigstatsr::big_univLinReg(big_X, y, ncores = ncores))


#' Access FBM from another session
# -----------------------------------------
#' To share an FBM, you first save its metadata to an .rds file.
rds_path <- big_X$save() # This returns the path to the .rds file.

#' The following is a conceptual example of what you would run in a
#' **separate R session** to access the data.
#'
#' ```
#' # --- In a new R session ---
#' library(bigstatsr)
#'
#' # Use the path returned by big_X$save() in the first session
#' path_to_rds_file <- "PASTE_THE_PATH_HERE"
#'
#' # `big_attach` returns a new FBM object pointing to the same file.
#' attached_X <- big_attach(path_to_rds_file)
#'
#' # Changes made here are instantly reflected in the original session.
#' attached_X <- 42
#' ```