# Tutorial: bigstatsr
# ---------------------
# bigstatsR is an adaption of bigmemory and like it, it also provides a file
# backing based data type. However, compared to bigmemry, we can work with the
# objects directly, rather than their pointers.
# 
# bigstatsr more modern compared to bigmemory, and backing files are easier to
# delete. First delete the R obj andperform a gc(), then you can delete the
# backingfile.
# Also bigstats allows offers a C++ backend for own code.
# 
# The author's github:
# https://privefl.github.io/R-presentation/bigstatsr.html#19
# -----
library(bigstatsr)


# create big matrix obj
# ---------------------------
path_to_file <- paste0(getwd(), "/Backend/")
file_name <- "test_bigStatsR"
complete_file_path <- paste0(path_to_file, file_name)
bigX <- FBM(nrow = 10e2, ncol = 1e4, init = 0, backingfile = complete_file_path)
print(bigX)
class(bigX)
object.size(bigX)
file.size(bigX$backingfile)
typeof(bigX)


# bigstatsR offers apply functions, such as standard R's apply functions for
# big matrices (also in parallel)
# -------------------------------------
big_apply(bigX, a.FUN = function(bigX, ind) {
  bigX[, ind] <- rnorm(nrow(bigX) * length(ind))
  return(NULL) # we don't want to return anything
})
bigX[,]

# access some dummy data
# -------------------------
bigX[1:5, 1]
bigX[15:30, 25:50]


# Demo: big algorithms and performance
# --------------------------------------

# 1. Correlation matrix
# ---------------------
# first create R stack object than compute correlation matrix
mat <- bigX[]
system.time(corr1 <- cor(mat))

# apply big matrix function
system.time(corr2 <- big_cor(bigX))

all.equal(corr1, corr2[])
# The results coincide. NOTE: for big matrices big_cor outperforms cor!


# 2. Singular value decomposition
# ------------------------------
system.time(svd1 <- svd(scale(mat), nu = 10, nv = 10))

# quadratic in the smallest dimension, linear in the other one
system.time(svd2 <- big_SVD(bigX, fun.scaling = big_scale(), k = 10))

# extremely useful if both dimensions are very large
system.time(svd3 <- big_randomSVD(bigX, fun.scaling = big_scale(), k = 10))


# 3. Multiple association
# --------------------
M <- 100  # number of causal variables
set <- sample(ncol(bigX), M)
y <- scale(bigX[, set]) %*% rnorm(M)
y <- y + rnorm(length(y), sd = 2 *sd(y))
mult_test <- big_univLinReg(bigX, y, covar.train = svd2$u)
plot(mult_test)


library(ggplot2)
plot(mult_test, type = "Manhatta") +
  aes(color = cols_along(bigX) %in% set) +
  labs(color = "Causal?")


# 4. Prediction with big spLinReg
# -----------------------------
# Split the indices in train/test sets
ind_train <- sort(sample(nrow(bigX), size = 0.8*nrow(bigX)))
ind_test <- setdiff(rows_along(bigX), ind_train)

# train a linear model with elastic-net regularization and automatic choice of
# hyper-parameter lambda
big_splm_mod <- big_spLinReg(bigX, y[ind_train], ind.train = ind_train,
                             covar.train = svd2$u[ind_train, ],
                             alphas = c(1, 0.1, 0.01))

# get prediction for the test set
pred <- predict(big_splm_mod, X=bigX, ind.row=ind_test, covar.row=svd2$u[ind_test, ])

# plot true vs prediction
qplot(pred, y[ind_test]) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme_bigstatsr()


# 5. Compute the sum for each column
# ----------------------------------
# Brute force
sums1 <- colSums(bigX[])

# Block wise
sums2 <- big_apply(bigX, a.FUN = function(bigX, ind) {
  return(colSums(bigX[, ind]))
}, a.combine = 'c')

# check equality
all.equal(sums2, sums1)


# Parallelism
# ---------------------------------------------------
# Most functions of bigstatsR are parallized
# ------------------------------
ind_rep <- rep(cols_along(bigX), each = 100)
(NCORES <- nb_cores()) # number of physical cores

# serial vs parallel
# ---------------------
# serial
system.time(
  mult_test2 <- big_univLinReg(bigX, y, covar.train = svd2$u,
                               ind.col = ind_rep)
)

# parallel
system.time(
  mult_test3 <- big_univLinReg(bigX, y, covar.train = svd2$u,
                               ind.col = ind_rep, ncores = NCORES) 
)

# parallelize your own functions
system.time(
  mult_test4 <- big_parallelize(
    bigX, p.FUN = function(bigX, ind, y, covar) {
      bigstatsr::big_univLinReg(bigX, y, covar.train = covar,
                                ind.col = ind)
    }, p.combine = "rbind", ind = ind_rep, 
    ncores = NCORES, y = y, covar = svd2$u)
)

# => ability to run algorithms on 100 GBs of data


# alternative parallel access
# ---------------------------------
library(foreach)
cl <- parallel::makeCluster(10)
doParallel::registerDoParallel(cl)

foreach(j = 1:10, .combine = 'c') %dopar% {
  return(sum(bigX[, j]))
}
parallel::stopCluster(cl)


# Access FBM from another session
# -----------------------------------------
# save obj in .rds file to use in another session
bigX$is_saved
bigX$save()    # create a .rds file at the same folder as your .bk file
bigX$is_saved

# query the path to your .rds file, this is similar to bigmemory's .desc file
rds_path <- bigX$rds


# OPEN ANOTHER R session
### ------------------------------------------
path_to_rds <- "C:\\Users\\fabia\\Documents\\R\\Shared Memory\\Backend\\test_BigStatsR.rds"
# set active binding
big_X <- bigstatsr::big_attach(path_to_rds)
big_X[]
big_X[1:10, 1:3] <- big_X[1:10, 1:3] + 42
big_X[]
q()       # leave the new R session
### ------------------------------------------



# Main R session
# --------------------------------------------
# consequence of active binding
bigX[1:10, 1:3] # see the changes
typeof(bigX)

bigX[] <- rep(42, times = length(bigX))
gc()
bigX[, 1:6]
bigX[1, ]                 # row wise access not recommended, major column format
bigX[, c(TRUE, FALSE)]    # column access is much faster
bigX[cbind(1:10, 1:10)] <- NA_real_
bigX[1:10, 1:10]
gc()
bigX[cbind(1:10, 1:10)] <- 1
bigX[1:10, 1:10]
gc()



