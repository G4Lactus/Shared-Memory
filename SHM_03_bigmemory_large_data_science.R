#' bigmemory in data administration
# ------------------------------------------------------------------------------
#' The following tutorial places `bigmemory` in the context of data science
#'  in R. Detail is given to handling large data and then showcaseing how
#'  `bigmemory` integrates with other powerful packages, e.g., for
#'  parallel computing and large-scale statistical modeling.
#'
#'
#' What is "Large Data"?
#' -----------------------------------
#' The question what is large depends on the available hardware and task:
#'
#' - **Medium sized Data (e.g., < 2GB):** Fits into RAM, but processing can
#'    still be slow or memory-intensive. Optimized procedures are key.
#'    Try to reduce size of the file before loading it into R
#'    - for .xls files, select (a) specific column(s) required for analysis
#'    - for .csv and .txt files this is not possible
#'    - preprocess data on the command line using: `cat` or `awk`
#'
#' - **Large sized Data (e.g., > 4GB):** May not fit into R's available memory.
#'    This requires out-of-memory techniques, such as those provided by
#'    `bigmemory`'s file-backing.
#'
#' - **Very Large files (e.g., > 10 - 100 + x GB):** Often require distributed
#'    computing systems across multiple machines (e.g., SPARK, Dask)
#'



#' Strategy 1: Efficiently Reading Medium Data
# ------------------------------------------------------------------------------
#' Before resorting to out-of-memory packages, try to optimize data loading.
#' The `data.table::fread()` function is a best-in-class tool for this.
#'
library(data.table)

# Create a sample CSV file
data_dir <- file.path(tempdir(), "data_admin_demo")
dir.create(data_dir)
path_to_file <- file.path(data_dir, "test_data.csv")

test_data <- matrix(rnorm(1e6), nrow = 100)
data.table::fwrite(test_data, file = path_to_file)
rm(test_data)

#' **Benchmark: `read.csv` and `fread`**
#' `fread` is often magnitudes faster than R's `read.csv`.
#' It also provides powerful features like column selection (`select=`)
#'  directly from the file, a highly effective memory-saving technique.
#'
cat("Timing read.csv:\n")
system.time(read.csv(path_to_file))
cat("\nTiming data.table::fread:\n")
system.time(data.table::fread(path_to_file))



#' Strategy 2: Using `bigmemory` for Large Data
# ------------------------------------------------------------------------------
#' Load the CSV directly into a file-backed `big.matrix`.
#' The data is read from the text file and written to the binary backing file.
big_mtx <- bigmemory::read.big.matrix(
  filename = path_to_file,
  type = "double",
  header = TRUE,
  backingpath = data_dir
)

#' The object can now be used for processing
colsums_from_big_mtx <- colSums(big_mtx[, 1:5])
print(colsums_from_big_mtx)

#' Descriptor file -> can be used within other processes.
big_mtx_desc <- bigmemory::describe(big_mtx)



#' Strategy 3: Parallel Processing
# ------------------------------------------------------------------------------
#' For code that must run on any operating system (Windows, macOS, Linux), the
#' universal solution is a socket cluster.
#' This method, provided by R's parallel package, launches new R sessions and
#' communicates with them over network sockets.
#'
#' This approach is universally compatible but involves more communication
#' overhead than the fork method, as data must be explicitly sent to each
#' worker process.

#' Define the Worker Function
#' The function remains the same. It takes a column index and the descriptor
#' for the shared big.matrix.
worker_function <- function(col_idx, big_mtx_desc) {

  require(bigmemory)
  require(biganalytics)

  shared_matrix <- bigmemory::attach.big.matrix(big_mtx_desc)

  return(
    biganalytics::colrange(shared_matrix, cols = col_idx)
  )
}

library(parallel)

#' Detect the number of available cores and create a cluster that is one
#' less, which is a good practice for system stability.
num_cores <- parallel::detectCores() - 1

#' Create a PSOCK (Parallel Socket) cluster.
cl <- parallel::makePSOCKcluster(num_cores)
#' Unlike with a fork cluster, we must explicitly send the objects that our
#' worker function needs to each new R session.
parallel::clusterExport(cl, varlist = "big_mtx_desc")

#' Now, run the function in parallel.
results <- parallel::parSapply(cl, 1:5,
  worker_function,
  big_mtx_desc = big_mtx_desc
)

#' Always stop the cluster when you are finished to free up resources.
parallel::stopCluster(cl)

print(results)



#' Applications: The "Big" Ecosystem
# -----------------------------------------------------------

#' **`biganalytics`** for K-Means Clustering
#' Create a large with with 3 distinct clusters
big_random_mtx <- bigmemory::big.matrix(
  nrow = 3e6, ncol = 5, init = 0, type = "double"
)
big_random_mtx[seq(1, 3e6, by = 3), ] <- rnorm(5e6)
big_random_mtx[seq(2, 3e6, by = 3), ] <- rnorm(5e6, 1, 1)
big_random_mtx[seq(3, 3e6, by = 3), ] <- rnorm(5e6, -1, 1)

#' Run k-means directly on the `big.matrix` object
km_res <- biganalytics::bigkmeans(
  big_random_mtx, centers = 3, nstart = 3, iter.max = 10
)
print(km_res)


#' **`biglm` for Chunk-wise Linear Models**
#' The `biglm` package fits models to data that is too larget to load
#' at once by processing it in sequential "chunks".
#'
library(biglm)

#' Standard `lm` for comparison, fit on full `trees` dataset
data(trees)
lm_mod <- stats::lm(log(Volume) ~ log(Girth) + log(Height), data = trees)

#' Simulate reading the data in chunks
chunk1 <- trees[1:10, ]
chunk2 <- trees[11:20, ]
chunk3 <- trees[21:31, ]

#' Initialiaze the model with first chunk
biglm_mod <- biglm::biglm(log(Volume) ~ log(Girth) + log(Height), data = chunk1)

#' Update the model
biglm_mod <- update(biglm_mod, chunk2)
biglm_mod <- update(biglm_mod, chunk3)

#' NOTE:
#' - data has been chunked along rows
#' - initial fit is done with biglm
#' - model is updated with further chunks using update fct


#' Compare results
#' The coefficients are virtually identical
cat("\nComparison of lm and biglm coefficients:\n")
print(rbind(
  lm = coef(lm_mod),
  biglm = coef(biglm_mod)
))