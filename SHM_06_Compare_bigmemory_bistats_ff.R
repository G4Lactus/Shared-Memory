# Shared memory vs. parallel memory parallelism
# -------------------------------------------------
library(bigmemory)
library(bigstatsr)
library(ff)
library(parallel)

# Illustrate the performance of parallel code
# ---------------------------------------------
N <- 5e5
mat <- matrix(rnorm(N*2), N, 2)

temp_dir <- tempdir()
file <- "bigM"
big_mtx <- as.big.matrix(mat, type = "double", 
                         backingfile = paste0(file, ".bk"), 
                         backingpath = temp_dir, 
                         descriptorfile = paste0(file, ".desc")
                        )

fbm_mtx <- as_FBM(mat, type = "double",
                  backingfile = paste0(temp_dir, "\\", file, "_fbm")
                 )

ff_mtx <- ff(initdata = mat, dim = dim(mat), vmode = "double", 
             filename = paste0(temp_dir, "\\", file, ".ff"), 
             finalizer = "close"
             )


# shared memory and [big.memory]
# ---------------------------------------------
cl <- makeCluster(detectCores() - 2, type = "PSOCK")
part <- clusterSplit(cl, seq_len(N)) # work load balancing
time_1 <- system.time(
  res1 <- unlist(
    parLapply(cl, part, function(part, data_desc) {
      
      library(bigmemory)
      data <- attach.big.matrix(data_desc)
      dat <- abs(data[part, 1] - data[part, 2])
      dat <- as.big.matrix(dat)
      
      return(describe(dat))
    }, describe(big_mtx))
  )
)
stopCluster(cl)
gc()


# shared memory and [bigstatsr]
# ---------------------------------------------
cl <- makeCluster(detectCores() - 2, type = "PSOCK")
time_2 <- system.time(
  res2 <- unlist(
    parLapply(cl, part, function(part, fbm_mtx) {
      library(bigstatsr)
      return(fbm_mtx[part, 1] - fbm_mtx[part, 2])
    }, fbm_mtx)
  )
)
stopCluster(cl)
gc()



# shared memory and [ff] 
# ---------------------------------------------
cl <- makeCluster(detectCores() - 2, type = "PSOCK")
time_3 <- system.time(
  res3 <- unlist(
    parLapply(cl, part, function(x, ff_mtx) {
      library(ff)
      return(ff_mtx[x, 1] - ff_mtx[x, 2])
    }, ff_mtx)
  )
)
stopCluster(cl)
gc()



cl <- makeCluster(detectCores() - 2, type = "PSOCK")
time_4 <- system.time(
  res4 <- unlist(
    parLapply(cl, chunk(ff(1:N)), function(x, ff_mtx) {
      library(ff)
      return(ff_mtx[x, 1] - ff_mtx[x, 2])
    }, ff_mtx)
  )
)
stopCluster(cl)
gc()



library(snowfall)
sfInit(parallel = TRUE, cpus=10, type="SOCK")
sfLibrary(ff)
sfExport("ff_mtx")
sfClusterEval(open(ff_mtx))
time_5 <- system.time(
  sfLapply(chunk(1:N), function(x) {
    return(ff_mtx[x, 1] - ff_mtx[x, 2])
  })
)
sfStop()
gc()


# Timing performance
# ---------------------------------------------
rbind(time_1, time_2, time_3, time_4, time_5)

