#' Demo: Parallel Data Access, Race Conditions, and Solutions
# -----------------------------------------------------------------------------
#' This tutorial demonstrates how to use `big.matrix` objects in a parallel
#' context with teh `foreach` package.
#' Also, the critical problem of "data races" is demonstrated and strategies
#' for resolving them are discussed.
library(foreach)
library(parallel)
library(doParallel)
library(bigmemory)
library(flock)

# Cluster Setup
cl <- parallel::makeCluster(parallel::detectCores() - 2)
doParallel::registerDoParallel(cl)
foreach::getDoParWorkers()

# Temporary Directory Setup
temp_dir <- tempdir()



#' Scenario 1: Demonstrating the Data Race Problem
# -----------------------------------------------------------------------------
#' A data race occurs when multiple paralle workers attempt to read and write
#' to the exact same memory location without any coordination. The final result
#' is unpredictable, as it depends on the precise (and random) order in which
#' the workers access the memory.
#'

# Create file-backed big.matrix
backing_dir1 <- file.path(temp_dir, "scenario_1")
dir.create(backing_dir1)

big_mtx_1 <- bigmemory::as.big.matrix(
  x = matrix(0, 10, 10),
  type = "double",
  backingfile = "big_mtx_1.bin",
  backingpath = backing_dir1,
  descriptorfile = "big_mtx_1.desc"
)
big_mtx_desc_1 <- bigmemory::describe(big_mtx_1)

rm(big_mtx)
gc()


#' Parallel Loop with Data Race
foreach(i = 1:10, .packages = "bigmemory") %dopar% {
  # Each worker attaches the same matrix
  m <- bigmemory::attach.big.matrix(big_mtx_desc_1)

  # These writing accesses are safe, as each worker writes to its own
  # independent column 'i'.
  m[1, i] <- m[1, i] + 5
  m[3, i] <- m[3, i] + 42
  m[8, i] <- m[8, i] + exp(1)

  # Data Race: All workers try to update m[5, 5] at the same time.
  # Each worker reads the current value, adds 5, and writes it back.
  # If the value is 0, muliple workers might read 0, all calculate 5,
  # and all write 5 back. The final result will in general not be 50.
  m[5, 5] <- m[5, 5] + 5

  return(NULL)
}

#' Result inspection
m_tst_1 <- bigmemory::attach.big.matrix(big_mtx_desc_1)
m_tst_1[, ]

#' The value at m_tst_1[5,5] illustrates the data race outcome!
#' The expected result is 10 * 5 = 50, but the actual result is much smaller.
cat("Value at [5, 5]: ", m_tst_1[5, 5], "(Expected: 50)\n")

rm(list = setdiff(ls(), c("cl", "temp_dir")))
gc()

# -----------------------------------------------------------------------------



#' Scenario 2: Algorithmic Solution - The "Reduce" Strategy
#' -----------------------------------------------------------------------------
#' The most robust way to handle this data race inside a `foreach` context is to
#' avoid having workers write to a shared location.
#' The `.combine` argument of `foreach` then "reduces" (e.g., sums) these
#' results in a safe, serial manner.
#'
backing_dir2 <- file.path(temp_dir, "scenario_2")
dir.create(backing_dir2)

big_mtx_2 <- bigmemory::big.matrix(
  nrow = 10, ncol = 10, type = "double", init = 0,
  backingfile = "big_mtx_2.bin",
  backingpath = backing_dir2,
  descriptorfile = "big_mtx_2.desc"
)
big_mtx_2_desc <- bigmemory::describe(big_mtx_2)

# The "Reduce" strategy is achieved by using `.combine = 'sum'`
total_update_value <- foreach(
  i = 1:10,
  .packages = "bigmemory",
  .combine = "sum"
) %dopar% {

  m <- bigmemory::attach.big.matrix(big_mtx_2_desc)

  # Safe, independent writes
  m[1, i] <- m[1, i] + 5
  m[3, i] <- m[3, i] + 42
  m[8, i] <- m[8, i] + exp(1)

  # SOLUTION: Instead of writing to m[5, 5], an intermediate value
  # is computed and as per worker result returned.
  update_val <- 5

  return(update_val)
}

# The final update is then performed afterwards
m_tst_2 <- bigmemory::attach.big.matrix(big_mtx_2_desc)
m_tst_2[5, 5] <- m_tst_2[5, 5] + total_update_value
print(m_tst_2[, ])
cat("Value at [5, 5]: ", m_tst_2[5, 5], "(Expected: 50)\n")

rm(list = setdiff(ls(), c("cl", "temp_dir")))
gc()
# -----------------------------------------------------------------------------



#' Scenario 3: Mutex, lock and unlock
# -----------------------------------------------------------------------------
#' Another solution to the race problem is to enforce atomic execution,
#' i.e., only one thread changes data at a time, also called synchronization.
#' The guarding mechanism is called mutex (mutual exclusive lock).
#' The `flock` package provides a file-based locking.
backing_dir3 <- file.path(temp_dir, "scenario_3")
dir.create(backing_dir3)

big_mtx_3 <- bigmemory::as.big.matrix(
  x = matrix(0, 10, 10),
  type = "double",
  shared = TRUE,
  backingfile = "big_mtx_3.bin",
  backingpath = backing_dir3,
  descriptorfile = "big_mtx_3.desc",
)
big_mtx_3_desc <- bigmemory::describe(big_mtx_3)

# Create a separate, empty file to serve as lock
lock_file_path <- file.path(backing_dir3, "my.lock")
file.create(lock_file_path)


# Parallel Loop with Mutex
foreach(i = 1:10, .packages = c("bigmemory", "flock")) %dopar% {

  m <- bigmemory::attach.big.matrix(big_mtx_3_desc)

  # independent concurrent reading and writing on the same obj
  # ----------------------------------------------------------
  m[1, i] <- m[1, i] + 5
  m[3, i] <- m[3, i] + 42
  m[8, i] <- m[8, i] + exp(1)

  # Critical Section:.combine
  # A worker must acquire the lock before entering this block.
  # If the lock is held by another worker, it will wait.
  my_lock <- flock::lock(lock_file_path)
  # This section is now atomic: only one worker can execute it
  # at a time
  m[5, 5] <- m[5, 5] + 5

  flock::unlock(my_lock)

  return(NULL)
}

#' Result inspection
m_tst_3 <- attach.big.matrix(big_mtx_3_desc)
print(m_tst_3[, ])
cat("Value at [5, 5]:", m_tst_3[5, 5], "(Expected: 50)\n")
# The result is now correct. However, this method introduces overhead,
# as workers may have to wait for the lock. The algorithmic solution in
# Scenario 2 is often more efficient for `foreach` loops.

# Stop Cluster
parallel::stopCluster(cl)
