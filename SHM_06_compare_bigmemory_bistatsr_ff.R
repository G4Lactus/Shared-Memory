#' Tutorial 6: A Performance Showdown
# -----------------------------------------------------------------------------
#' The following tutorial benchmarks: `bigmemory`, `bigstatsr`, and `ff`.
#'
#' The test case is a common parallel task: performing a simple calculation
#' on each row of a large matrix. This will highlight the differences in API
#' design, parallelization strategy, and raw performance for each package.
# -----------------------------------------------------------------------------
library(bigmemory)
library(bigstatsr)
library(ff)
library(parallel)

#' Setup: Create the Data and On-Disk Objects
#' We'll use a 500,000 x 2 matrix as our source data.
n_elems <- 5e5
mat <- matrix(rnorm(n_elems * 2), n_elems, 2)

#' Create a temporary directory to store all backing files.
backing_dir <- file.path(tempdir(), "benchmark_demo")
dir.create(backing_dir, showWarnings = FALSE)

#' Create the file-backed object for each package.
#' bigmemory
big_mtx_desc <- describe(as.big.matrix(
  mat,
  backingfile = "big_mtx.bin",
  descriptorfile = "big_mtx.desc",
  backingpath = backing_dir
))

#' bigstatsr
fbm_mtx <- as_FBM(mat, backingfile = file.path(backing_dir, "fbm_mtx"))
fbm_mtx$save()
fbm_rds_path <- fbm_mtx$rds

#' ff
#' We use finalizer = "close" to prevent workers from deleting the file.
ff_mtx <- ff(
  initdata = mat,
  dim = dim(mat),
  filename = file.path(backing_dir, "ff_mtx.ff"),
  finalizer = "close"
)



#' Run the Benchmarks
#' Set up a cross-platform parallel cluster.
cl <- makeCluster(detectCores() - 2, type = "PSOCK")

#' Benchmark `bigmemory`:
#' The worker function attaches to the matrix via its descriptor.
time_bigmemory <- system.time({
  res_bigmemory <- unlist(parLapply(cl, clusterSplit(cl, 1:n_elems),
    fun = function(part, desc) {
      require(bigmemory)
      m <- attach.big.matrix(desc)
      return(abs(m[part, 1] - m[part, 2]))
    },
    desc = big_mtx_desc
  ))
})


#' Benchmark `bigstatsr`:
#' The worker function attaches to the FBM via its .rds file path.
time_bigstatsr <- system.time({
  res_bigstatsr <- unlist(parLapply(cl, clusterSplit(cl, 1:n_elems),
    fun = function(part, rds) {
      require(bigstatsr)
      m <- big_attach(rds)
      return(abs(m[part, 1] - m[part, 2]))
    },
    rds = fbm_rds_path
  ))
})


#' Benchmark `ff`:
#' Export the name of the ff object and have each worker `open()` it.
clusterExport(cl, "ff_mtx")
clusterEvalQ(cl, {
  require(ff)
  open(ff_mtx)
})

time_ff <- system.time({
  res_ff <- unlist(parLapply(cl, chunk(1:n_elems),
    fun = function(part) {
      return(abs(ff_mtx[part, 1] - ff_mtx[part, 2]))
    }
  ))
})


#' Shutdown
stopCluster(cl)
gc()



#' Display Results
#' Combine the timing results into a clean, readable data frame.
timing_results <- data.frame(
  Package = c("bigmemory", "bigstatsr", "ff"),
  Elapsed_Time_sec = c(
    time_bigmemory["elapsed"],
    time_bigstatsr["elapsed"],
    time_ff["elapsed"]
  )
)
print(timing_results)
